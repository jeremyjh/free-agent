{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, ScopedTypeVariables #-}


module FreeAgent.Client.Peer
    ( queryPeerServers
    , queryPeerCount
    , callTarget
    , castTarget
    , registerServer
    , PeerCommand(..)
    , peerServerName
    , warmRemoteCache
    ) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Process
import           FreeAgent.Core.Protocol

import           Data.Binary




data PeerCommand = DiscoverPeers
                   | QueryPeerCount
                   | QueryLocalServices
                   | RegisterPeer Peer
                   | RespondRegisterPeer Peer
                   | RegisterServer String ProcessId
    deriving (Show, Typeable, Generic)

instance Binary PeerCommand
instance NFData PeerCommand


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

-- | Query a remote node string ("host:port") for its registered
-- servers, and register them locally as resolving RemoteCache would,
-- so that multiple remote whereis do not have to be sent for
-- different servers at the same node.
warmRemoteCache :: forall process. MonadProcess process
                => String -> process ()
warmRemoteCache nodestr = liftProcess $
 do let nodeId = makeNodeId nodestr
    pid <- getSelfPid
    nsendRemote nodeId peerServerName (QueryLocalServices, pid)
    mservers <- expectTimeout 1000000 :: Process (Maybe [ServerRef])
    case mservers of
        Nothing -> say "warmRemoteCache timed out!"
        Just servers' ->
            forM_ servers' $ \(ServerRef name' pid') ->
                register (nodestr ++ name') pid'
