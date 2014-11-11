{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies                   #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                               #-}


module FreeAgent.Server.Executive
    ( execServer
    , StoreAction(..)
    , QueryActions(..)
    , UnregisterAction(..)
    , ExecuteStored(..)
    , ExecuteBatch(..)
    , AddListener(..)
    , executeStored
    , executeAction
    , matchAction
    , matchResult
    , serverName
    , ExecFail(..)
    )
where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Action
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Database.AcidState
import           FreeAgent.Process                  as Process
import           FreeAgent.Server.Executive.History hiding (serverName)
import           FreeAgent.Server.ManagedAgent


import           Control.Monad.Reader               (ask)
import Control.Monad.State (StateT)
import           Data.Binary
import qualified Data.Map.Strict                    as Map

import           Control.Distributed.Static         (unclosure)
import           Control.Error                      (hoistEither, (!?), (??))
import           Data.Default                       (Default (..))

-- -----------------------------
-- Types
-- -----------------------------
type RunningActions = Map Key ProcessId

type StoredAction = (Action, UTCTime)

data ExecPersist
  = ExecPersist { _persistActions   :: Map Key StoredAction
                , _persistListeners :: [Closure Listener]
                } deriving (Show, Typeable)

makeFields ''ExecPersist

instance Default ExecPersist where
    def = ExecPersist mempty []


data ExecFail = ECallFailed CallFail
              | EDeserializationFailure Text
              | ActionNotFound Key
              | ActionFailed !Text
              | DBException !Text
              deriving (Show, Eq, Typeable, Generic)

type ExecAgent a = EitherT ExecFail (StateT ExecState Agent) a

data ExecState
  = ExecState { _stateRunning   :: !RunningActions
              , _stateListeners :: ![Listener]
              , _stateAcid      :: !(AcidState ExecPersist)
              , _stateCachedActions  :: !(Map Key (ExecAgent Result))
              , _stateExecutedCount  :: !Int
              } deriving (Typeable, Generic)


instance Binary ExecFail

instance Convertible RunnableFail ExecFail where
    safeConvert = return . ActionFailed . tshow

instance Convertible CallFail ExecFail where
    safeConvert = return . ECallFailed

data ExecutiveCommand =
        ExecuteAction Action
      | TerminateExecutive
      deriving (Show, Typeable, Generic)

instance Binary ExecutiveCommand where
instance NFData ExecutiveCommand where

-- -----------------------------
-- Persistent state functions
-- -----------------------------

putAction :: StoredAction -> Update ExecPersist ()
putAction stored@(action', _) =
    actions %= Map.insertWith newer (key action') stored
  where newer new@(_, ntime) old@(_,otime)
          | ntime > otime = new
          | otherwise = old

deleteAction :: Key -> Update ExecPersist ()
deleteAction key' =
    actions %= Map.delete key'

getAction :: Key -> Query ExecPersist (Maybe StoredAction)
getAction key' = views actions (Map.lookup key')

allActions :: Query ExecPersist [Action]
allActions = views actions (fmap (fst . snd) . Map.toList)

putListener :: Closure Listener -> Update ExecPersist ()
putListener listener = listeners %= (:) listener

getPersist :: Query ExecPersist ExecPersist
getPersist = ask

-- we have to make the splices near the top of the file
$(makeAcidic ''ExecPersist ['putAction, 'getAction, 'deleteAction,'allActions, 'putListener
                           ,'getPersist])

makeFields ''ExecState
-- -----------------------------
-- API
-- -----------------------------

data StoreAction = StoreAction Action
                 | StoreNewerAction Action UTCTime
                 | StoreActions [Action]
      deriving (Show, Typeable, Generic)


instance Binary StoreAction

cacheAction :: Action -> StateT ExecState Agent ()
cacheAction action' = cachedActions %= Map.insert (key action') (doExec action')

instance ServerCall StoreAction where
    type CallState StoreAction = ExecState
    type CallResponse StoreAction = ()
    callName _ = serverName
    respond (StoreAction action')  =
        do now <- getCurrentTime
           void $ update (PutAction (action',now))
           cacheAction action'
    respond (StoreNewerAction action' time) =
     do void $ update (PutAction (action', time))
        cacheAction action'
    respond (StoreActions actions')  =
        do now <- getCurrentTime
           forM_ actions' $ \action' ->
            do void $ update (PutAction (action',now))
               cacheAction action'

data UnregisterAction = UnregisterAction !Key
      deriving (Show, Typeable, Generic)

instance Binary UnregisterAction

instance ServerCall UnregisterAction where
    type CallState UnregisterAction = ExecState
    callName _ = serverName
    respond (UnregisterAction key')  =
     do void $ update (DeleteAction key')
        cachedActions %= Map.delete key'


data ExecuteStored = ExecuteStored !Key
    deriving (Show, Typeable, Generic)

instance Binary ExecuteStored

instance ServerCall ExecuteStored where
    type CallState ExecuteStored = ExecState
    type CallResponse ExecuteStored = Either ExecFail Result
    callName _ = serverName
    respond cmd@(ExecuteStored key') =
     do executedCount %= (+) 1
        runLogEitherT cmd $
            join (uses cachedActions (lookup key') !? ActionNotFound key')


instance ServerCast ExecuteStored where
    type CastState ExecuteStored = ExecState
    castName _ = serverName
    handle cmd =
     do executedCount %= (+) 1
        void . spawnLocal . void $ respond cmd

data ExecuteBatch = ExecuteBatch ![Key]
    deriving (Show, Typeable, Generic)

instance Binary ExecuteBatch
instance NFData ExecuteBatch where rnf = genericRnf

instance ServerCast ExecuteBatch where
    type CastState ExecuteBatch = ExecState
    castName _ = serverName
    handle (ExecuteBatch keys) =
        forM_ keys $ \key' ->
         do executedCount %= (+) 1
            void . spawnLocal . void $ respond (ExecuteStored key')

data AddListener = AddListener (Closure Listener)
    deriving (Show, Typeable, Generic)

instance Binary AddListener
instance NFData AddListener

instance ServerCast AddListener where
    type CastState AddListener = ExecState
    castName _ = serverName
    handle (AddListener cl) =
      do rt <- viewConfig remoteTable
         case unclosure rt cl of
             Left msg -> [qwarn|AddListener failed! Could not evaluate
                           new listener closure: #{msg}|]
             Right listener -> do
                 listeners %= (:) listener
                 update (PutListener cl)

data QueryActions = QueryActions
    deriving (Show, Eq, Typeable, Generic)

instance Binary QueryActions
instance NFData QueryActions

instance ServerCall QueryActions where
    type CallState QueryActions = ExecState
    type CallResponse QueryActions = [Action]
    callName _ = serverName
    respond QueryActions  = query AllActions

-- | Execute 'Action' corresponding to 'Key' with the current
-- target server and extract the concrete result (if possible) to
-- the expected type.
executeStored :: (MonadAgent agent, Resulting result)
                  => Key -> agent (Either ExecFail result)
executeStored key' =
    runEitherT $
     do result' <- callResult >>= convEitherT >>= hoistEither
        extract result' ?? EDeserializationFailure (tshow result')
  where callResult = lift $ callServ $ ExecuteStored key'

executeAction :: (MonadAgent agent, Runnable a b)
              => a -> agent (Either ExecFail Result)
executeAction action' =
    callExecutive $ ExecuteAction (toAction action')

-- | Used with 'addListener' - defines a 'Listener' that will
-- receive a 'Result' for each 'Action' executed that matches the typed predicate
-- argument. Only predicates for Actions in which the underlying concrete type
-- matches will be evaluated.
matchAction :: Runnable a b => (a -> Bool) -> NodeId -> String -> Listener
matchAction af = ActionMatching (matchA af)

-- | Used with 'addListener' - defines a 'Listener' that will
-- receive a 'Result' for each 'Action' executed where both the Action and Result
-- match the predicate arguments.
-- If you need the 'ActionMatcher' predicate to have access to the underlying
-- concrete type, then pass the typed predicate to 'matchA' to make an
-- 'ActionMatcher'.
matchResult :: Resulting b
            => ActionMatcher -> (b -> Bool) -> NodeId -> String -> Listener
matchResult af rf = ResultMatching af (matchR rf)

serverName :: String
serverName = "agent:executive"

callExecutive :: (NFSerializable a, MonadAgent agent)
            => ExecutiveCommand -> agent (Either ExecFail a)
callExecutive command = do
    eresult <- callTarget serverName command
    case eresult of
        Right result' -> return result'
        Left cf -> return (Left $ ECallFailed cf)

-- -----------------------------
-- Implementation
-- -----------------------------

execServer :: AgentServer
execServer =
    defineServer
        serverName
        initExec
        defaultProcess {
            apiHandlers =
            [ agentRpcAsyncHandlerET $
                  \cmd -> case cmd of
                      ExecuteAction act -> doExecuteAction act
                      _ -> $(err "illegal pattern match")

            , registerCall (Proxy :: Proxy StoreAction)
            , registerCall (Proxy :: Proxy UnregisterAction)
            , registerCall (Proxy :: Proxy ExecuteStored)
            , registerCall (Proxy :: Proxy QueryActions)
            , registerCast (Proxy :: Proxy ExecuteStored)
            , registerCast (Proxy :: Proxy ExecuteBatch)
            , registerCast (Proxy :: Proxy AddListener)
            ]
          , shutdownHandler = \(AgentState _ s) _ -> do
                pid <- getSelfPid
                say $ "Executed # Actions: " ++ show (s ^. executedCount)
                say $ "Executive server " ++ show pid ++ " shutting down."
        }
  where initExec = do
            listeners' <- join $ viewContext $ plugins.listeners
            acid' <- openOrGetDb "agent-executive" def def
            persist <- query' acid' GetPersist
            rt <- viewConfig remoteTable
            let cls = rights $ map (unclosure rt) (persist ^. listeners)
            let caches = map (doExec . fst) (persist ^. actions)
            return $ ExecState (Map.fromList []) (listeners' ++ cls) acid' caches 0



doExecuteAction :: Action -> ExecAgent Result
doExecuteAction = doExec

doExec :: Action -> ExecAgent Result
doExec action' = do
    result <- tryExec action' >>= convEitherT
    storeResult result >>= notifyListeners
  where
    storeResult result = do
        [qdebug| Storing result: #{result}|]
        writeResult result >>= convEitherT
        return result
    notifyListeners result = do
        listeners' <- uses listeners (filter fst . map exMatch)
        [qdebug| Checking match for #{length listeners'} listeners |]
        forM_ listeners' $ \(_,addr) -> do
            [qdebug|Sending Result: #{result} To: #{addr}|]
            send addr result
        return result
      where
        exMatch (ActionMatching afilter nodeid name') =
            (afilter action', (nodeid,name'))
        exMatch (ResultMatching afilter rfilter nodeid name') =
            (afilter action' && rfilter result, (nodeid, name'))


deriveSafeStore ''ExecPersist

instance NFData ExecFail where rnf = genericRnf
instance NFData StoreAction where rnf = genericRnf
instance NFData UnregisterAction where rnf = genericRnf
instance NFData ExecuteStored where rnf = genericRnf
