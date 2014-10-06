{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}

-- | Provides MonadProcess typeclass and lifted versions of commonly used
-- functions
module FreeAgent.Process
    ( module Control.Distributed.Process
    , module Control.Distributed.Process.Closure
    , module FreeAgent.Process
    , NFSerializable
    , localNodeId
    , ChildSpec
    , Async, AsyncResult(..)
    )
where

import           Control.Monad.Reader                                             (ReaderT, mapReaderT)
import           Control.Monad.State                                              (StateT, mapStateT)
import qualified Control.Monad.State                                              as State
import           Control.Monad.Trans                                              (MonadIO,
                                                                                   lift)

import           Control.Concurrent.Lifted                                        (threadDelay)
import           Control.Distributed.Process                                      hiding
                                                                                   (call,
                                                                                   expect, expectTimeout, getSelfPid,
                                                                                   nsend, register,
                                                                                   send, sendChan, spawnLocal, whereis)
import qualified Control.Distributed.Process                                      as Base
import           Control.Distributed.Process.Closure                              (mkClosure, remotable)
import           Control.Distributed.Process.MonadBaseControl                     ()
import           Control.Distributed.Process.Node                                 (localNodeId)
import qualified Control.Distributed.Process.Node                                 as Node
import           Control.Distributed.Process.Platform                             (Addressable, NFSerializable, Resolvable, Routable (..))
import qualified Control.Distributed.Process.Platform                             as Platform (__remoteTable)
import qualified Control.Distributed.Process.Platform                             as Base
import           Control.Distributed.Process.Platform.Async                       (Async, AsyncResult (..))
import qualified Control.Distributed.Process.Platform.Async                       as Base
import qualified Control.Distributed.Process.Platform.ManagedProcess.UnsafeClient as Managed
import           Control.Distributed.Process.Platform.Supervisor                  (ChildSpec)
import qualified Control.Distributed.Process.Platform.UnsafePrimitives            as NF
import           Control.Distributed.Process.Serializable                         (Serializable)
import           Control.Error                                                    (EitherT)
import           Control.Monad.Trans.Control                                      (MonadBaseControl (..))
import           Data.Tuple                                                       (swap)

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
     do s <- State.get
        flip mapStateT ma $ \ mas ->
            let stripped = fmap fst mas
                applied = mapProcess f stripped
                tack = swap . (,) s
            in fmap tack applied

instance MonadProcess m => MonadProcess (EitherT e m) where
    liftProcess = lift . liftProcess
    mapProcess = error "not implemented correctly"
         --TODO problem here is the bind we do on ma'
         --means the application of f happens after the computation runs
         --when semantically it needs to be before that to support e.g.
         --spawnLocal correctly.
    {-mapProcess f ma = error "not implemented correctly"-}
        {-flip mapEitherT ma $-}
        {-\ma' -> do-}
            {-ea <- ma'-}
            {-case ea of-}
                {-Left e -> return $ Left e-}
                {-Right a -> do-}
                    {-b <- mapProcess f (return a)-}
                    {-return $ Right b-}

initRemoteTable :: RemoteTable
initRemoteTable = Platform.__remoteTable Node.initRemoteTable

spawnLocal :: (MonadProcess m) => m () -> m ProcessId
spawnLocal = mapProcess Base.spawnLocal

spawnLinkLocal :: (MonadProcess m) => m () -> m ProcessId
spawnLinkLocal = mapProcess Base.spawnLinkLocal

getSelfPid :: (MonadProcess m) => m ProcessId
getSelfPid = liftProcess Base.getSelfPid

-- | Send a processId a message - note this is actually
-- 'Routable.unsafeSendTo' from Control.Distributed.Process.Platform
send :: (MonadProcess m, NFSerializable a, Routable addr) => addr -> a -> m ()
send addr = liftProcess . unsafeSendTo addr


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

waitRegistration :: MonadProcess m => String -> m ProcessId
waitRegistration sname = loop
  where loop = whereis sname >>= maybe (threadDelay 1000 >> loop) return

resolve :: (MonadProcess m, Resolvable addr)
        => addr -> m (Maybe ProcessId)
resolve = liftProcess . Base.resolve

async :: (Serializable a, MonadProcess m) => m a -> m (Async a)
async = mapProcess Base.async

wait :: MonadProcess process
     => Async a -> process (AsyncResult a)
wait = liftProcess . Base.wait
