{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


module FreeAgent.Executive where

import           FreeAgent.Prelude
import           FreeAgent.Lenses
import           FreeAgent.Action
import           FreeAgent.Database

import           Data.Binary
import qualified Data.Map.Strict as Map

import           Control.Distributed.Process.Lifted as Process
import           Control.Distributed.Process.Platform.ManagedProcess



-- -----------------------------
-- Types
-- -----------------------------
type RunningActions = Map Key ProcessId
type ActionListeners = Map Key [ProcessId]

data ExecState
  = ExecState { _stateRunning :: RunningActions
              , _stateListeners :: ActionListeners
              } deriving (Show, Eq, Typeable, Generic)

initialState :: ExecState
initialState = ExecState (Map.fromList []) (Map.fromList [])
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
                      deriving (Show, Eq, Typeable, Generic)
instance Binary ExecutiveCommand where

-- -----------------------------
-- API
-- -----------------------------

-- | initialize the executive server (call once at startup)
init :: (MonadAgent m) => m ProcessId
init = do
    pid <- spawnLocal mainLoop
    Process.register registeredAs pid
    return pid

-- | send a command to the executive process for execution
execute :: (MonadAgent m) => ExecutiveCommand -> m ()
execute cmd = do
    (Just pid) <- whereis registeredAs
    send pid cmd

-- -----------------------------
-- Implementation
-- -----------------------------

mainLoop :: (MonadAgent m) => m ()
mainLoop = do
    command <- expect
    case command of
        TerminateExecutive -> logM "shutting down"
        cmd -> do
            void $ spawnLocal $ perform cmd
            mainLoop

executiveProcess :: ProcessDefinition ExecState
executiveProcess = defaultProcess {
       apiHandlers = [
         handleCall  (\s (n :: Int) -> reply (n * 2) s)
       , handleCast  (\s (_ :: ExecutiveCommand) -> continue s )
       ]
    }

-- | execute a particular command and send the response
perform :: (MonadAgent m) => ExecutiveCommand -> m ()
perform (RegisterAction a)  = registerAction a
perform (UnregisterAction k) = unRegisterAction k
perform (ExecuteAction a) = executeAction a
perform (ExecuteRegistered k) = executeRegistered k
perform _ = undefined
{-perform (QueryActionHistory query) = undefined-}

-- ExecutiveCommand realizations
registerAction :: (MonadAgent m) => Action -> m ()
registerAction = withAgentDB . withSync . stashAction

unRegisterAction :: (MonadAgent m) => Key -> m ()
unRegisterAction = withAgentDB . withSync . deleteAction

executeAction :: (MonadAgent m) => Action -> m ()
executeAction = void . spawnLocal . doExec

executeRegistered :: (MonadProcess m, ConfigReader m) => Key -> m ()
executeRegistered k = void $ spawnLocal $ do
    act <- fromAgentDB $ fetchAction k
    case act of
        Right a -> doExec a
        Left (ParseFail msg) ->
            logM ("Unable to retrieve action key: " ++ toT k ++ " - " ++ toT msg)
        Left (NotFound _) ->
            logM ("Unable to retrieve action key: " ++ toT k ++ " not found in database.")

doExec :: (MonadProcess m, ConfigReader m) => Action -> m ()
doExec act = exec act >>=
    either logM
           (\result -> withAgentDB $
                withKeySpace (resultKS act) $
                    store (key result) result)

registerEvent :: (MonadLevelDB m) => Event -> m ()
registerEvent = withEventKS . withSync . stash

unRegisterEvent :: (MonadLevelDB m) => Key -> m ()
unRegisterEvent = withEventKS . withSync . delete

registeredAs :: String
registeredAs = "Executive"

-- TODO: higher-leveldb needs to let us supply a function
-- that will change the current options rather than overriding them
withSync :: (MonadLevelDB m) => m () -> m()
withSync = withOptions (def, def {sync = True})

resultKS :: (Actionable a b) => a -> KeySpace
resultKS a = "agent:actions:" ++ key a

-- TODO: use real logging
logM :: (MonadIO m) => Text -> m()
logM = putStrLn

withEventKS :: (MonadLevelDB m) => m () -> m ()
withEventKS = withKeySpace "agent:events"
