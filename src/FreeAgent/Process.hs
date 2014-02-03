{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provides MonadProcess typeclass and lifted versions of commonly used
-- functions
module FreeAgent.Process
    ( module Control.Distributed.Process
    , module FreeAgent.Process
    , NFSerializable
    , ChildSpec
    )
where

import           Control.Monad.Reader                                  (ReaderT, mapReaderT)
import           Control.Monad.Trans                                   (lift, MonadIO)
import           Control.Monad.RWS                                     (RWST, Monoid)
import           Control.Monad.State                                   (StateT, mapStateT)

import           Control.Distributed.Process                           hiding
                                                                        (expect, expectTimeout, getSelfPid,
                                                                        nsend, register,
                                                                        send, spawnLocal,
                                                                        call,
                                                                        whereis, sendChan)
import qualified Control.Distributed.Process                           as Base
import           Control.Distributed.Process.Internal.Types            (Process(..), LocalProcess(..))
import           Control.Distributed.Process.Platform                  (NFSerializable, Addressable)
import           Control.Distributed.Process.Serializable              (Serializable)
import qualified Control.Distributed.Process.Platform                  as Base (spawnLinkLocal)
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as NF
import qualified Control.Distributed.Process.Platform.ManagedProcess.UnsafeClient   as Managed
import           Control.Distributed.Process.Platform.Supervisor (ChildSpec)
import           Control.Monad.Base                                    (MonadBase(..))
import           Control.Monad.Trans.Control                           (MonadBaseControl(..))
import           Control.Monad.Trans.Resource                          (MonadThrow(..), MonadUnsafeIO(..))

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
    mapProcess = id

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

spawnLinkLocal :: (MonadProcess m) => m () -> m ProcessId
spawnLinkLocal = mapProcess Base.spawnLinkLocal

getSelfPid :: (MonadProcess m) => m ProcessId
getSelfPid = liftProcess Base.getSelfPid

-- | Send a processId a message - note this is actually the "unsafe" version
-- from Control.Distributed.Process.Platform.UnsafePrimitives
send :: (MonadProcess m, NFSerializable a) => ProcessId -> a -> m ()
send pid = liftProcess . NF.send pid


-- | Send a processId a message - note this is actually the "unsafe" version
-- from Control.Distributed.Process.Platform.UnsafePrimitives
sendChan :: (MonadProcess m, NFSerializable a) => SendPort a -> a -> m ()
sendChan port = liftProcess . NF.sendChan port

syncCallChan :: forall s a b m. (MonadProcess m, Addressable s, NFSerializable a, NFSerializable b) => s -> a -> m b
syncCallChan port = liftProcess . Managed.syncCallChan port

-- | Send a named process a message - note this is actually the "unsafe" version
-- from Control.Distributed.Process.Platform.UnsafePrimitives
nsend :: (MonadProcess m, NFSerializable a) => String -> a -> m ()
nsend name = liftProcess . NF.nsend name

expect :: (MonadProcess m) => forall a. Serializable a => m a
expect = liftProcess Base.expect

expectTimeout :: (MonadProcess m) => forall a. Serializable a => Int -> m (Maybe a)
expectTimeout = liftProcess . Base.expectTimeout

register :: (MonadProcess m) => String -> ProcessId -> m ()
register name = liftProcess . Base.register name

whereis :: (MonadProcess m) => String -> m (Maybe ProcessId)
whereis = liftProcess . Base.whereis

-- | Cast to a managed process server - this is the "unsafe" version
cast :: (Addressable s, MonadProcess m, NFSerializable a)
     => s -> a -> m ()
cast pid = liftProcess . Managed.cast pid

-- | Call to a managed process server - this is the "unsafe" version
call :: (Addressable s, MonadProcess m
        ,NFSerializable a, NFSerializable b) => s -> a -> m b
call pid = liftProcess . Managed.call pid
