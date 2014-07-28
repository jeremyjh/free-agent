{-# LANGUAGE NoImplicitPrelude      #-}

module FreeAgent.Server.Peer
    ( callServer
    , castServer
    , CallFail(..)
    )

where

import           AgentPrelude
import           FreeAgent.Process
import           FreeAgent.Core.Internal.Types
import           Data.Binary (Binary)

data CallFail = RoutingFailed | ServerCrash String
instance Show CallFail
instance Binary CallFail
instance Typeable CallFail
instance NFData CallFail

callServer :: (MonadAgent agent, NFSerializable a, NFSerializable b)
           => String -> a -> agent (Either CallFail b)

castServer :: (MonadAgent agent, NFSerializable a)
           => String -> a -> agent (Either CallFail ())
