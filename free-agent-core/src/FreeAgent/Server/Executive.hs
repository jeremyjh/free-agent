{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}


module FreeAgent.Server.Executive
    ( execServer
    , StoreAction(..)
    , UnregisterAction(..)
    , ExecuteStored(..)
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
import           FreeAgent.Database.AcidState
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Process                     as Process
import           FreeAgent.Server.ManagedAgent
import           FreeAgent.Server.Executive.History    hiding (serverName)


import           Control.Monad.State                   (StateT)
import           Control.Monad.Reader (ask)
import           Data.Binary
import qualified Data.Map.Strict                       as Map

import           Control.Distributed.Static            (unclosure)
import           Control.Error                         ((!?), hoistEither, (??))
import           Data.Default                          (Default(..))

-- -----------------------------
-- Types
-- -----------------------------
type RunningActions = Map Key ProcessId

data ExecPersist
  = ExecPersist { _persistActions :: Map Key Action
                , _persistListeners :: [Closure Listener]
                } deriving (Show, Typeable)

makeFields ''ExecPersist

instance Default ExecPersist where
    def = ExecPersist mempty []

data ExecState
  = ExecState { _stateRunning   :: !RunningActions
              , _stateListeners :: ![Listener]
              , _stateAcid      :: !(AcidState ExecPersist)
              } deriving (Typeable, Generic)

data ExecFail = ECallFailed CallFail
              | EDeserializationFailure Text
              | ActionNotFound Key
              | ActionFailed !Text
              | DBException !Text
              deriving (Show, Eq, Typeable, Generic)

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

putAction :: Action -> Update ExecPersist ()
putAction action' =
    actions %= Map.insert (key action') action'

deleteAction :: Key -> Update ExecPersist ()
deleteAction key' =
    actions %= Map.delete key'

getAction :: Key -> Query ExecPersist (Maybe Action)
getAction key' = views actions (Map.lookup key')

putListener :: Closure Listener -> Update ExecPersist ()
putListener listener = listeners %= (:) listener

getPersist :: Query ExecPersist ExecPersist
getPersist = ask

-- we have to make the splices near the top of the file
$(makeAcidic ''ExecPersist ['putAction, 'getAction, 'deleteAction, 'putListener
                           ,'getPersist])

-- -----------------------------
-- API
-- -----------------------------

data StoreAction = StoreAction Action
      deriving (Show, Typeable, Generic)

instance Binary StoreAction

instance ServerCall StoreAction () ExecState where
    callName _ = serverName
    respond (StoreAction action')  =
        update (PutAction action')

data UnregisterAction = UnregisterAction !Key
      deriving (Show, Typeable, Generic)

instance Binary UnregisterAction

instance ServerCall UnregisterAction () ExecState where
    callName _ = serverName
    respond (UnregisterAction key')  =
        update (DeleteAction key')


data ExecuteStored = ExecuteStored !Key
    deriving (Show, Typeable, Generic)

instance Binary ExecuteStored

instance ServerCall ExecuteStored (Either ExecFail Result) ExecState where
    callName _ = serverName
    respond = handleET $ \(ExecuteStored key') ->
     do action' <-  query (GetAction key') !? ActionNotFound key'
        doExec action'

instance ServerCast ExecuteStored ExecState where
    castName _ = serverName
    handle = void . respond

data AddListener = AddListener (Closure Listener)
    deriving (Show, Typeable, Generic)

instance Binary AddListener
instance NFData AddListener

instance ServerCast AddListener ExecState where
    castName _ = serverName
    handle (AddListener cl) =
     do rt <- viewConfig remoteTable
        case unclosure rt cl of
            Left msg -> [qwarn|AddListener failed! Could not evaluate
                          new listener closure: #{msg}|]
            Right listener -> do
                listeners %= (:) listener
                update (PutListener cl)

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
            , registerCast (Proxy :: Proxy ExecuteStored)
            , registerCast (Proxy :: Proxy AddListener)
            ]
        }
  where initExec = do
            listeners' <- join $ viewContext $ plugins.listeners
            acid' <- openOrGetDb "agent-executive" def def
            persist <- query' acid' GetPersist
            rt <- viewConfig remoteTable
            let cls = rights $ map (unclosure rt) (persist^.listeners)
            return $ ExecState (Map.fromList []) (listeners' ++ cls) acid'


type ExecAgent a = EitherT ExecFail (StateT ExecState Agent) a

-- -----------------------------
-- Executive command realizations
-- -----------------------------


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
makeFields ''ExecState
instance NFData ExecFail where rnf = genericRnf
instance NFData StoreAction where rnf = genericRnf
instance NFData UnregisterAction where rnf = genericRnf
instance NFData ExecuteStored where rnf = genericRnf
