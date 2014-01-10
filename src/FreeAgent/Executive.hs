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


module FreeAgent.Executive where

import           FreeAgent.Action
import           FreeAgent.Core                                      (withAgent)
import           FreeAgent.Database
import           FreeAgent.Lenses
import           AgentPrelude

import           Data.Binary
import qualified Data.Map.Strict                                     as Map

import           Control.Monad.State                                 (StateT, evalStateT)

import           Control.Distributed.Process.Lifted                  as Process
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform                (whereisOrStart)


-- -----------------------------
-- Types
-- -----------------------------
type RunningActions = Map Key ProcessId

data ExecState
  = ExecState { _stateRunning         :: !RunningActions
              , _stateActionListeners :: ![ActionListener]
              , _stateResultListeners :: ![ResultListener]
              } deriving (Typeable, Generic)
makeFields ''ExecState

-- -----------------------------
-- Types
-- -----------------------------

data ExecutiveCommand = RegisterAction Action
                      | UnregisterAction Key
		              | ExecuteAction Action
		              | ExecuteRegistered Key
                      | DeliverPackage Package
                      | RemovePackage UUID
                      {-| QueryActionHistory (ScanQuery ActionHistory [ActionHistory])-}
                      | TerminateExecutive
                      deriving (Typeable, Generic)
instance Binary ExecutiveCommand where
instance NFData ExecutiveCommand where

-- -----------------------------
-- API
-- -----------------------------
registerAction :: (MonadAgent m) => Action -> m ()
registerAction = sendCommand . RegisterAction

executeRegistered :: (MonadAgent m) => Key -> m ()
executeRegistered = sendCommand . ExecuteRegistered

executeAction :: (MonadAgent m) => Action -> m ()
executeAction = sendCommand . ExecuteAction

-- | Send / register a package and optional list of Actions to register
-- to each context leader defined in the package
deliverPackage :: (MonadAgent m) => Package -> m ()
deliverPackage p = sendCommand $ DeliverPackage p

-- | Remove a package definition from the context leader(s). Does
-- not unregister Actions or remove history.
removePackage :: (MonadAgent m) => UUID -> m ()
removePackage uuid = sendCommand $ RemovePackage uuid

-- -----------------------------
-- Implementation
-- -----------------------------

-- | send a command to the executive process for execution
sendCommand :: (MonadAgent m) => ExecutiveCommand -> m ()
sendCommand cmd = do
    ctxt <- askContext
    pid <- liftProcess $ whereisOrStart "executive" (withAgent ctxt init)
    send pid cmd

init :: Agent ()
init = do
    alisteners <- join $ viewConfig actionListeners
    rlisteners <- join $ viewConfig resultListeners
    let _state = ExecState (Map.fromList []) alisteners rlisteners
    evalStateT loop _state
  where
    loop = do
        command <- expect
        case command of
            TerminateExecutive -> logM "shutting down"
            cmd -> do
                void $ spawnLocal $ perform cmd
                loop

type ExecAgent a = StateT ExecState Agent a

instance (ContextReader m)
      => ContextReader (StateT ExecState m) where
    askContext = lift askContext

executiveProcess :: ProcessDefinition ExecState
executiveProcess = defaultProcess {
       apiHandlers = [
         handleCall  (\s (n :: Int) -> reply (n * 2) s)
       , handleCast  (\s (_ :: ExecutiveCommand) -> continue s )
       ]
    }

-- | sendCommand a particular command and send the response
perform :: ExecutiveCommand -> ExecAgent ()
perform (RegisterAction a)  = doRegisterAction a
perform (UnregisterAction k) = doUnRegisterAction k
perform (ExecuteAction a) = doExecuteAction a
perform (ExecuteRegistered k) = doExecuteRegistered k
perform (DeliverPackage p) = doDeliverPackage p
perform (RemovePackage uuid) = doRemovePackage uuid
perform _ = undefined
{-perform (QueryActionHistory query) = undefined-}

-- ExecutiveCommand realizations
doRegisterAction ::  Action -> ExecAgent ()
doRegisterAction act = do
    $(logDebug) $ "Registering action: " ++ tshow act
    void . agentDb $ stashAction act

doUnRegisterAction :: Key -> ExecAgent ()
doUnRegisterAction k = do
    $(logDebug) $ "Unregistering action key: " ++ tshow k
    agentDb $ deleteAction k

doExecuteAction :: Action -> ExecAgent ()
doExecuteAction act = do
    $(logDebug) $ "Executing action: " ++ tshow act
    void $ spawnLocal $ doExec act

doExecuteRegistered :: Key -> ExecAgent ()
doExecuteRegistered k = void $ spawnLocal $ do
    $(logDebug) $ "Executing action key: " ++ tshow k
    act <- agentDb $ fetchAction k
    case act of
        Right a -> do
            $(logDebug) $ "Executing action: " ++ tshow a
            doExec a
        Left (ParseFail msg) ->
            $(logWarn) $ "Unable to retrieve action key: " ++ toT k
                         ++ " - " ++ toT msg
        Left (NotFound _) ->
            $(logWarn) $ "Unable to retrieve action key: " ++ toT k
                        ++ " not found in database."

doExec :: Action -> ExecAgent ()
doExec act =
    exec act >>=
    either $(logWarn) (storeResult >=> notifyListeners)
  where
    storeResult result = do
        asyncAgentDb $ withKeySpace (resultKS act) $
            store (timestampKey result) result
        return result
    notifyListeners result = do
        alisteners <- use actionListeners
        forM_ alisteners $ \(afilter, apid) ->
            when (afilter act) $ do
                $(logDebug) $ "Sending Result: " ++ tshow result
                send apid result
        rlisteners <- use resultListeners
        forM_ rlisteners $ \(rfilter, rpid) ->
            when (rfilter result) $ send rpid result
    resultKS a = "agent:actions:" ++ key a
    timestampKey = utcToBytes . view timestamp . summary

doDeliverPackage :: Package -> ExecAgent ()
doDeliverPackage package = do
    --TODO: Figure out what it even means to "route"
    $(logDebug) $ "Delivering package: " ++ tshow package
    registerIt >>= storeIt >>= scheduleIt
  where
    registerIt = do
        keys <- forM (package^.actions) $ \act -> do
            doRegisterAction act
            return $ key act
        return $ package & actionKeys .~ (keys ++ (package^.actionKeys))
                         & actions .~ []
    storeIt = agentDb . withPackageKS . stash
    scheduleIt pkg =
        case pkg^.schedule of
            Now ->
                forM_ (pkg^.actionKeys) $ \akey ->
                    doExecuteRegistered akey
            _ -> return () --TODO: add to scheduler

doRemovePackage :: UUID -> ExecAgent ()
doRemovePackage uuid = do
    $(logDebug) $ "Unregistering package: " ++ tshow uuid
    agentDb . withPackageKS . delete $ toBytes uuid

withPackageKS :: (MonadLevelDB m) => m a -> m a
withPackageKS = withKeySpace "agent:packages"
