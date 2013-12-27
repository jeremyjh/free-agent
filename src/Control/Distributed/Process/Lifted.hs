{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Distributed.Process.Lifted
    ( module Control.Distributed.Process
    , module Control.Distributed.Process.Lifted
    , NFSerializable
    )
where

import           Control.Monad.Trans.Control
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Base
import           Control.Monad.Trans.Resource
import qualified Control.Distributed.Process as Base
import           Control.Distributed.Process
    hiding ( getSelfPid, send, expect, expectTimeout, spawnLocal
           , register, whereis, nsend)
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as NF
import           Control.Distributed.Process.Platform (NFSerializable)
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
class (MonadIO m, MonadBaseControl IO m) => MonadProcess m where
    -- |lift a base 'Process' computation into the current monad
    liftProcess :: Process a -> m a
    -- |map over an underlying Process to e.g. lift spawnLocal
    mapProcess :: (Process a -> Process b) -> m a -> m b

instance MonadProcess Process where
    liftProcess = id
    mapProcess f = f

instance (Monad m, MonadProcess m) => MonadProcess (ReaderT r m) where
    liftProcess = lift . liftProcess
    mapProcess f = mapReaderT (mapProcess f)

instance (Monad m, MonadProcess m) => MonadProcess (StateT s m) where
    liftProcess = lift . liftProcess
    mapProcess f ma =
        flip mapStateT ma $
        \ma' -> do
            (a, s) <- ma'
            b <- mapProcess f (return a)
            return (b, s)


-- example transformer implementation
instance (Monoid w, Monad m, MonadProcess m) => MonadProcess (RWST r w s m) where
    liftProcess = lift . liftProcess
    mapProcess = undefined
    {-f ma = -}
        {-flip mapRWST (trace "hit map1" ma) $-}
        {-\ma' -> do-}
            {-(a, s, w) <- (trace "hit map2" ma')-}
            {-b <- mapProcess (trace "hit map3" f) (return a)-}
            {-return (b, s, w)-}

spawnLocal :: (MonadProcess m) => m () -> m ProcessId
spawnLocal = mapProcess Base.spawnLocal

getSelfPid :: (MonadProcess m) => m ProcessId
getSelfPid = liftProcess Base.getSelfPid

-- | Send a processId a message - note this is actually the "unsafe" version
-- from Control.Distributed.Process.Platform.UnsafePrimitives
send :: (MonadProcess m, NFSerializable a) => ProcessId -> a -> m ()
send pid = liftProcess . NF.send pid

-- | Send a named process a message - note this is actually the "unsafe" version
-- from Control.Distributed.Process.Platform.UnsafePrimitives
nsend :: (MonadProcess m, NFSerializable a) => String -> a -> m ()
nsend name = liftProcess . NF.nsend name

expect :: (MonadProcess m) => forall a. NFSerializable a => m a
expect = liftProcess Base.expect

expectTimeout :: (MonadProcess m) => forall a. NFSerializable a => Int -> m (Maybe a)
expectTimeout = liftProcess . Base.expectTimeout

register :: (MonadProcess m) => String -> ProcessId -> m ()
register name = liftProcess . Base.register name

whereis :: (MonadProcess m) => String -> m (Maybe ProcessId)
whereis = liftProcess . Base.whereis
