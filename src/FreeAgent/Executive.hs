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
import           FreeAgent.Core                                      ()
import           FreeAgent.Database
import           FreeAgent.Lenses
import           FreeAgent.Prelude

import           Data.Binary
import qualified Data.Map.Strict                                     as Map

import           Control.Monad.State                                 (StateT, evalStateT)

import           Control.Distributed.Process.Lifted                  as Process
import           Control.Distributed.Process.Platform.ManagedProcess


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
                      {-| QueryActionHistory (ScanQuery ActionHistory [ActionHistory])-}
                      | RegisterEvent Event
                      | UnregisterEvent Event
                      {-| QueryEventHistory (ScanQuery EventHistory [EventHistory])-}
                      | TerminateExecutive
                      deriving (Typeable, Generic)
instance Binary ExecutiveCommand where
instance NFData ExecutiveCommand where

-- -----------------------------
-- API
-- -----------------------------

-- | initialize the executive server (call once at startup)
init :: Agent ProcessId
init = do
    alisteners <- join $ viewConfig actionListeners
    rlisteners <- join $ viewConfig resultListeners
    let _state = ExecState (Map.fromList []) alisteners rlisteners
    pid <- spawnLocal $ evalStateT loop _state
    Process.register registeredAs pid
    return pid
  where
    loop = do
        command <- expect
        case command of
            TerminateExecutive -> logM "shutting down"
            cmd -> do
                void $ spawnLocal $ perform cmd
                loop

-- | send a command to the executive process for execution
execute :: (MonadAgent m) => ExecutiveCommand -> m ()
execute = nsend registeredAs

-- -----------------------------
-- Implementation
-- -----------------------------

type ExecAgent a = StateT ExecState Agent a

instance (ConfigReader m)
      => ConfigReader (StateT ExecState m) where
    askConfig = lift askConfig

executiveProcess :: ProcessDefinition ExecState
executiveProcess = defaultProcess {
       apiHandlers = [
         handleCall  (\s (n :: Int) -> reply (n * 2) s)
       , handleCast  (\s (_ :: ExecutiveCommand) -> continue s )
       ]
    }

-- | execute a particular command and send the response
perform :: ExecutiveCommand -> ExecAgent ()
perform (RegisterAction a)  = registerAction a
perform (UnregisterAction k) = unRegisterAction k
perform (ExecuteAction a) = executeAction a
perform (ExecuteRegistered k) = executeRegistered k
perform _ = undefined
{-perform (QueryActionHistory query) = undefined-}

-- ExecutiveCommand realizations
registerAction ::  Action -> ExecAgent ()
registerAction = withAgentDB . withSync . stashAction

unRegisterAction :: Key -> ExecAgent ()
unRegisterAction = withAgentDB . withSync . deleteAction

executeAction :: Action -> ExecAgent ()
executeAction = void . spawnLocal . doExec

executeRegistered :: Key -> ExecAgent ()
executeRegistered k = void $ spawnLocal $ do
    act <- fromAgentDB $ fetchAction k
    case act of
        Right a -> doExec a
        Left (ParseFail msg) ->
            logM ("Unable to retrieve action key: " ++ toT k ++ " - " ++ toT msg)
        Left (NotFound _) ->
            logM ("Unable to retrieve action key: " ++ toT k ++ " not found in database.")

doExec :: Action -> ExecAgent ()
doExec act =
    $(logDebug) ("Executing action: " ++ tshow act) >>
    exec act >>=
    either $(logWarn) (storeResult >=> notifyListeners)
  where
    storeResult result = do
        withAgentDB $ withKeySpace (resultKS act) $
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

registerEvent :: Event -> ExecAgent ()
registerEvent = withAgentDB . withEventKS . withSync . stash

unRegisterEvent :: Key -> ExecAgent ()
unRegisterEvent = withAgentDB . withEventKS . withSync . delete

registeredAs :: String
registeredAs = "Executive"

withEventKS :: (MonadLevelDB m) => m () -> m ()
withEventKS = withKeySpace "agent:events"
