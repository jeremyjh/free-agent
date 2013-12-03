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

import           Control.Distributed.Process.Lifted
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import qualified Database.LevelDB.Higher as DB

import qualified Data.Map.Strict as Map


data ActionsInProgress = Map Key ProcessId

data ExecutiveCommand = RegisterAction Action
                      | UnregisterAction Key
		              | ExecuteAction Action
                      {-| QueryActionHistory (ScanQuery ActionHistory [ActionHistory])-}
                      | RegisterEvent Event
                      | UnregisterEvent Event
                      {-| QueryEventHistory (ScanQuery EventHistory [EventHistory])-}
                      deriving (Show, Eq, Typeable)
deriveBinary ''ExecutiveCommand

executiveProcess :: ProcessDefinition ActionsInProgress
executiveProcess = defaultProcess {
       apiHandlers = [
         handleCall  (\s (n :: Int) -> reply (n * 2) s)
       , handleCast  (\s (cmd :: ExecutiveCommand) -> continue s )
       ]
    }

listen :: (MonadProcess m, MonadLevelDB m) => m ProcessId
listen = undefined -- we need to just start-out with distributed process platform

-- | execute a particular command and send the response
perform :: (MonadAgent m) => ExecutiveCommand -> m ()
perform (RegisterAction a)  = registerAction a
perform (UnregisterAction k) = unRegisterAction k
perform (ExecuteAction a) = do (Right _) <- exec a; return ()
{-perform (QueryActionHistory query) = undefined-}

-- ExecutiveCommand realizations
registerAction :: (MonadLevelDB m) => Action -> m ()
registerAction = withActionKS . withSync . stash

unRegisterAction :: (MonadLevelDB m) => Key -> m ()
unRegisterAction = withActionKS . withSync . DB.delete

registerEvent :: (MonadLevelDB m) => Event -> m ()
registerEvent = withEventKS . withSync . stash

unRegisterEvent :: (MonadLevelDB m) => Key -> m ()
unRegisterEvent = withEventKS . withSync . DB.delete

-- TODO: higher-leveldb needs to let us supply a function
-- that will change the current options rather than overriding them
withSync :: (MonadLevelDB m) => m () -> m()
withSync = DB.withOptions (def, def {DB.sync = True})


withEventKS :: (MonadLevelDB m) => m () -> m ()
withEventKS = DB.withKeySpace "agent:events"
