{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- Resolvable (Target,String)

module FreeAgent.Client.Peer
    ( queryPeerServers
    , queryPeerCount
    , callTarget
    , castTarget
    , CallFail(..)
    , registerServer
    , PeerCommand(..)
    , peerServerName
    ) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Process
import           FreeAgent.Core.Internal.Lenses

import qualified Data.Set as Set
import           Data.Binary

import qualified Control.Distributed.Process.Platform as Platform

import           Control.Error ((!?))

data CallFail = RoutingFailed | ServerCrash String
        deriving (Show, Eq, Typeable, Generic)

instance Binary CallFail
instance Convertible SomeException CallFail where
    safeConvert e = return $ ServerCrash (show e)

data PeerCommand = DiscoverPeers
                   | QueryPeerCount
                   | RegisterPeer Peer
                   | RespondRegisterPeer Peer
                   | RegisterServer String ProcessId
                   | QueryPeerServers String (Set Context) (Set Zone)
    deriving (Show, Typeable, Generic)

instance Binary PeerCommand
instance NFData PeerCommand

callTarget :: (MonadAgent agent, NFSerializable a, NFSerializable b)
           => String -> a -> agent (Either CallFail b)
callTarget name' command = runEitherT $ do
    target <- viewContext targetServer
    pid <- resolve (target, name') !? RoutingFailed
    tryAny (syncCallChan pid command) >>= convEitherT

castTarget :: (MonadAgent agent, NFSerializable a)
           => String -> a -> agent (Either CallFail ())
castTarget name' command = runEitherT $ do
    target <- viewContext targetServer
    pid <- resolve (target, name') !? RoutingFailed
    cast pid command

peerServerName :: String
peerServerName = "agent:peer"

-- | Advertise a Server on the peer
registerServer :: (MonadAgent agent)
               => AgentServer -> ProcessId -> agent (Either CallFail ())
registerServer (AgentServer sname _) pid =
    castTarget peerServerName $ RegisterServer sname pid

-- | Get a list of Peers (from local Peer's cache)
-- which have the requested Server registered
-- Contexts and Zones
queryPeerServers :: MonadAgent agent
                => String -> Set Context -> Set Zone
                -> agent (Either CallFail (Set Peer))
queryPeerServers s c z = callTarget peerServerName $ QueryPeerServers s c z

-- | Returns the number of Peer Agent processes this node is connected to
queryPeerCount :: MonadAgent agent => agent (Either CallFail Int)
queryPeerCount = callTarget peerServerName QueryPeerCount

instance Platform.Resolvable (Target, String) where
    resolve (Local, name') = resolve name'
    resolve (ForeignNode nodeId, name') = resolve (nodeId, name')
    resolve (Remote peer, name') = resolve (peer, name')
    resolve (Route contexts' zones', name') = do
        peers <- queryLocalPeerServers name'
                                  (Set.fromList contexts')
                                  (Set.fromList zones')
        foundPeer peers
      where foundPeer peers
                | peers == Set.empty = return Nothing
                | otherwise = let peer:_ = Set.toList peers in resolve (peer, name')

queryLocalPeerServers :: MonadProcess process
                => String -> Set Context -> Set Zone
                -> process (Set Peer)
queryLocalPeerServers s c z = syncCallChan (Local, peerServerName) $ QueryPeerServers s c z

instance NFData CallFail where rnf = genericRnf
