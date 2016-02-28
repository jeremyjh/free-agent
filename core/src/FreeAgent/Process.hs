{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Lifted versions of some Platform functions and a few miscellaneous
-- utility functions.
module FreeAgent.Process
    ( module Control.Distributed.Process.Lifted
    , module Control.Distributed.Process.Closure
    , module FreeAgent.Process
    , NFSerializable
    , localNodeId
    , ChildSpec
    , makeNodeId
    , Async, AsyncResult(..)
    )
where

import Control.Monad.Trans(lift)
import           Control.Concurrent.Lifted                                        (threadDelay)
import           Control.Distributed.Process.Lifted.Class
import           Control.Distributed.Process.Lifted                                      hiding
                                                                                   (call,
                                                                                   nsend,
                                                                                   send, sendChan)
import           Control.Distributed.Process.Closure                              (mkClosure, remotable)
import           Control.Distributed.Process.MonadBaseControl                     ()
import           Control.Distributed.Process.Node                                 (localNodeId)
import qualified Control.Distributed.Process.Node                                 as Node
import           Control.Distributed.Process.Extras                               (Addressable, NFSerializable, Resolvable, Routable (..))

import           Control.Distributed.Process.Async                       (Async, AsyncResult (..))
import qualified Control.Distributed.Process.Extras                               as Extras
import qualified Control.Distributed.Process.Async                                as Base
import qualified Control.Distributed.Process.ManagedProcess.UnsafeClient as Managed
import           Control.Distributed.Process.Supervisor                  (ChildSpec)
import qualified Control.Distributed.Process.Extras.UnsafePrimitives            as NF
import           Control.Distributed.Backend.P2P      (makeNodeId)
import           Control.Error                        (EitherT)


instance MonadProcess m => MonadProcess (EitherT e m) where
    liftP = lift . liftP

initRemoteTable :: RemoteTable
initRemoteTable = Extras.__remoteTable Node.initRemoteTable


-- | Send a processId a message - note this is actually
-- 'Routable.unsafeSendTo' from Control.Distributed.Process.Platform
send :: (MonadProcess m, NFSerializable a, Routable addr) => addr -> a -> m ()
send addr = liftP . unsafeSendTo addr


-- | Send a processId a message - note this is actually the "unsafe" version
-- from Control.Distributed.Process.Platform.UnsafePrimitives
sendChan :: (MonadProcess m, NFSerializable a) => SendPort a -> a -> m ()
sendChan port = liftP . NF.sendChan port

syncCallChan :: forall s a b m. (MonadProcess m, Addressable s, NFSerializable a, NFSerializable b) => s -> a -> m b
syncCallChan port = liftP . Managed.syncCallChan port

-- | Send a named process a message - note this is actually the "unsafe" version
-- from Control.Distributed.Process.Platform.UnsafePrimitives
nsend :: (MonadProcess m, NFSerializable a) => String -> a -> m ()
nsend name = liftP . NF.nsend name

-- | Cast to a managed process server - this is the "unsafe" version
cast :: (Addressable s, MonadProcess m, NFSerializable a)
     => s -> a -> m ()
cast pid = liftP . Managed.cast pid

-- | Call to a managed process server - this is the "unsafe" version
call :: (Addressable s, MonadProcess m
        ,NFSerializable a, NFSerializable b) => s -> a -> m b
call pid = liftP . Managed.call pid

waitRegistration :: MonadProcess m => String -> m ProcessId
waitRegistration sname = loop
  where loop = whereis sname >>= maybe (threadDelay 1000 >> loop) return

resolve :: (MonadProcess m, Resolvable addr)
        => addr -> m (Maybe ProcessId)
resolve = liftP . Extras.resolve

wait :: MonadProcess process
     => Async a -> process (AsyncResult a)
wait = liftP . Base.wait
