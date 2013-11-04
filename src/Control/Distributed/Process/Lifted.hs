{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Distributed.Process.Lifted
    ( module Control.Distributed.Process
    , module Control.Distributed.Process.Lifted
    )
where

import           Control.Monad.Trans.Control
import           Control.Monad.Reader
import           Control.Monad.Base
import           Control.Monad.Trans.Resource
import qualified Control.Distributed.Process as Base
import           Control.Distributed.Process
    hiding (getSelfPid, send, expect, expectTimeout)

import           Control.Distributed.Process.Serializable
import           Control.Distributed.Process.Internal.Types


-- instances required under ResourceT
deriving instance MonadThrow Process
deriving instance MonadUnsafeIO Process
deriving instance MonadBase IO Process

instance MonadBaseControl IO Process where
  newtype StM Process a = StProcess {unSTProcess :: StM (ReaderT LocalProcess IO) a}
  restoreM (StProcess m) = Process $ restoreM m
  liftBaseWith f = Process $ liftBaseWith $ \ rib -> f (fmap StProcess . rib . unProcess)

-- lifted versions of Process functions
class MonadProcess m where
    liftProcess :: Process a -> m a

instance MonadProcess Process where
    liftProcess = id

getSelfPid :: (MonadProcess m) => m ProcessId
getSelfPid = liftProcess $ Base.getSelfPid

send :: (MonadProcess m, Serializable a) => ProcessId -> a -> m ()
send pid = liftProcess . Base.send pid

expect :: (MonadProcess m) => forall a. Serializable a => m a
expect = liftProcess $ Base.expect

expectTimeout :: (MonadProcess m) => forall a. Serializable a => Int -> m (Maybe a)
expectTimeout = liftProcess . Base.expectTimeout
