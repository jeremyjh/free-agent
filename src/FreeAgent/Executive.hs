{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}


module FreeAgent.Executive where

import           FreeAgent.Prelude
import           FreeAgent.Lenses
import           FreeAgent.Action

import           Data.Binary

import           Control.Distributed.Process.Lifted as Process
import           Control.Distributed.Process.Platform.ManagedProcess
import qualified Database.LevelDB.Higher as DB



-- -----------------------------
-- Types
-- -----------------------------
data ActionsInProgress = Map Key ProcessId

data ExecutiveCommand = RegisterAction Action
                      | UnregisterAction Key
		              | ExecuteAction Action
                      {-| QueryActionHistory (ScanQuery ActionHistory [ActionHistory])-}
                      | RegisterEvent Event
                      | UnregisterEvent Event
                      {-| QueryEventHistory (ScanQuery EventHistory [EventHistory])-}
                      | TerminateExecutive
                      deriving (Show, Eq, Typeable)
deriveBinary ''ExecutiveCommand

-- -----------------------------
-- API
-- -----------------------------

-- | initialize the executor server (call once at startup)
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
        cmd -> void $ spawnLocal $ perform cmd >> mainLoop

executiveProcess :: ProcessDefinition ActionsInProgress
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
perform _ = undefined
{-perform (QueryActionHistory query) = undefined-}

-- ExecutiveCommand realizations
registerAction :: (MonadLevelDB m) => Action -> m ()
registerAction = withSync . stashAction

unRegisterAction :: (MonadLevelDB m) => Key -> m ()
unRegisterAction = withSync . deleteAction

executeAction :: (MonadAgent m) => Action -> m ()
executeAction act = void $ spawnLocal $ do
    eresult <- exec act
    case eresult of
        Left msg -> logM msg
        Right result -> do
            DB.withKeySpace (resultKS act) $
                DB.store (key result) result

registerEvent :: (MonadLevelDB m) => Event -> m ()
registerEvent = withEventKS . withSync . stash

unRegisterEvent :: (MonadLevelDB m) => Key -> m ()
unRegisterEvent = withEventKS . withSync . DB.delete

registeredAs :: String
registeredAs = "Executive"

-- TODO: higher-leveldb needs to let us supply a function
-- that will change the current options rather than overriding them
withSync :: (MonadLevelDB m) => m () -> m()
withSync = DB.withOptions (def, def {DB.sync = True})

resultKS :: (Actionable a b) => a -> KeySpace
resultKS a = "agent:actions:" ++ key a

-- TODO: use real logging
logM :: (MonadIO m) => Text -> m()
logM = putStrLn

withEventKS :: (MonadLevelDB m) => m () -> m ()
withEventKS = DB.withKeySpace "agent:events"
