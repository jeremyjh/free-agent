{-# LANGUAGE ConstraintKinds        #-}
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
    , registerAction
    , unregisterAction
    , executeRegistered
    , executeRegisteredAsync
    , executeAction
    , addListener
    , matchAction
    , matchResult
    , serverName
    , ExecFail(..)
    )
where

import           AgentPrelude
import           FreeAgent.Action
import           FreeAgent.Database.AcidState
import           FreeAgent.Lenses
import           FreeAgent.Process                                   as Process
import           FreeAgent.Process.ManagedAgent
import           FreeAgent.Server.Executive.History                  hiding (serverName)
import           FreeAgent.Server.Peer

import           Control.DeepSeq.TH                                  (deriveNFData)
import           Control.Monad.State                                 (StateT)
import Control.Monad.Reader (ask)
import           Data.Binary
import qualified Data.Map.Strict                                     as Map

import           Control.Distributed.Static                          (unclosure)
import           Control.Error                                       hiding (err)
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
        RegisterAction Action
      | UnregisterAction Key
      | ExecuteAction Action
      | ExecuteRegistered Key
      {-| QueryActionHistory (ScanQuery ActionHistory [ActionHistory])-}
      | TerminateExecutive
      | AddListener (Closure Listener)
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

registerAction :: (MonadProcess m, Actionable a b)
               => Target -> a -> m (Either ExecFail ())
registerAction target action' =
    callExecutive target $ RegisterAction (toAction action')

unregisterAction :: (MonadProcess m) => Target -> Key -> m (Either ExecFail ())
unregisterAction target key' =
    callExecutive target $ UnregisterAction key'

executeRegistered :: (MonadProcess m)
                  => Target -> Key -> m (Either ExecFail Result)
executeRegistered target key' =
    callExecutive target $ ExecuteRegistered key'

executeRegisteredAsync :: (MonadProcess m)
                       => Target -> Key -> m (Either CallFail ())
executeRegisteredAsync target key' =
    castServer serverName target $ ExecuteRegistered key'

executeAction :: (MonadProcess m, Actionable a b)
              => Target -> a -> m (Either ExecFail Result)
executeAction target action' =
    callExecutive target $ ExecuteAction (toAction action')

-- | Asynchronously subscribe to a local or remote Executive service
-- Throws an exception if a Route is supplied which cannot be resolved
addListener :: (MonadProcess m)
            => Target -> Closure Listener -> m (Either CallFail ())
addListener target cl = castServer serverName target (AddListener cl)

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


callExecutive :: (NFSerializable a, MonadProcess m)
            => Target -> ExecutiveCommand -> m (Either ExecFail a)
callExecutive target command = do
    eresult <- callServer serverName target command
    case eresult of
        Right result' -> return result'
        Left cf -> return (Left $ ECallFailed cf)

-- -----------------------------
-- Implementation
-- -----------------------------

execServer :: AgentServer
execServer =
    defineServer serverName
                 initExec
                 defaultProcess {
                     apiHandlers =
                     [ handleRpcChan $ agentAsyncCallHandlerET $
                           \cmd -> case cmd of
                               ExecuteAction act -> doExecuteAction act
                               ExecuteRegistered k -> doExecuteRegistered k
                               _ -> $(err "illegal pattern match")

                     , handleRpcChan $ agentAsyncCallHandlerET $ \cmd -> doRegistration cmd

                     , handleCast $ agentCastHandlerET $ \ cmd ->
                           case cmd of
                               (AddListener cl) -> do
                                   rt <- viewConfig remoteTable
                                   case unclosure rt cl of
                                       Left msg -> [qwarn|AddListener failed! Could not evaluate
                                                        new listener closure: #{msg}|]
                                       Right listener -> do
                                           listeners %= (:) listener
                                           update (PutListener cl)
                               (ExecuteRegistered k) -> void $ spawnLocal $
                                   void $ doExecuteRegistered k
                               _ -> $(err "illegal pattern match")
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

doRegistration :: ExecutiveCommand -> ExecAgent ()
doRegistration (RegisterAction action')  =
    update (PutAction action')
doRegistration (UnregisterAction key') =
    update (DeleteAction key')
doRegistration _ = $(err "illegal pattern match")

doExecuteAction :: Action -> ExecAgent Result
doExecuteAction = doExec

doExecuteRegistered :: Key -> ExecAgent Result
doExecuteRegistered key' = do
    action' <-  query (GetAction key') !? ActionNotFound key'
    doExec action'

doExec :: Action -> ExecAgent Result
doExec action' = do
    result <- exec action' >>= convEitherT
    storeResult result >>= notifyListeners
  where
    storeResult result = do
        [qdebug| Storing result: #{result}|]
        writeResult Local result >>= convEitherT
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
deriveNFData ''ExecFail
