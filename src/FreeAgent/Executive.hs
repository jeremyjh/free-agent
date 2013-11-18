{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module FreeAgent.Executive where

import           FreeAgent.Prelude
import           FreeAgent.Lenses
import           FreeAgent.Action

import           Control.Distributed.Process.Lifted
import           Database.LevelDB.Higher


listen :: (MonadProcess m, MonadLevelDB m) => m ProcessId
listen = undefined -- we need to just start-out with distributed process platform

-- | execute a particular command and send the response
perform :: (MonadAgent m) => ExecutiveCommand -> m ()
perform (RegisterAction a)  = registerAction a
perform (UnregisterAction k) = unRegisterAction k
perform (ExecuteAction a) = do (Right _) <- exec a; return ()
perform (QueryActionHistory query) = undefined

-- ExecutiveCommand realizations
registerAction :: (MonadLevelDB m) => WrappedAction -> m ()
registerAction = withActionKS . withSync . stash

unRegisterAction :: (MonadLevelDB m) => Key -> m ()
unRegisterAction = withActionKS . withSync . delete

registerEvent :: (MonadLevelDB m) => Event -> m ()
registerEvent = withEventKS . withSync . stash

unRegisterEvent :: (MonadLevelDB m) => Key -> m ()
unRegisterEvent = withEventKS . withSync . delete

-- TODO: higher-leveldb needs to let us supply a function
-- that will change the current options rather than overriding them
withSync :: (MonadLevelDB m) => m () -> m()
withSync = withOptions (def, def {sync = True})

withActionKS :: (MonadLevelDB m) => m () -> m ()
withActionKS = withKeySpace "agent:actions"

withEventKS :: (MonadLevelDB m) => m () -> m ()
withEventKS = withKeySpace "agent:events"
