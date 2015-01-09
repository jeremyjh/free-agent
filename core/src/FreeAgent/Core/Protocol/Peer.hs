{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, ScopedTypeVariables #-}
{-# LANGUAGE CPP, TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- ServerCall QueryPeerServers

module FreeAgent.Core.Protocol.Peer
    ( queryPeerServers
    , queryPeerCount
    , callTarget
    , castTarget
    , registerServer
    , PeerImpl(..)
    , DiscoverPeers(..)
    , RegisterPeer(..)
    , RespondRegisterPeer(..)
    , RegisterServer(..)
    , QueryPeerCount(..)
    , QueryLocalServices(..)
    , peerServerName
    , warmRemoteCache
    ) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Process
import           FreeAgent.Core.Protocol

import           Data.Binary


data PeerImpl st = PeerImpl {
    castDiscoverPeers       :: DiscoverPeers -> ProtoT DiscoverPeers st ()
  , castRegisterPeer        :: RegisterPeer -> ProtoT RegisterPeer st ()
  , castRespondRegisterPeer :: RespondRegisterPeer -> ProtoT RespondRegisterPeer st ()
  , castRegisterServer      ::  RegisterServer -> ProtoT RegisterServer st ()
  , callQueryPeerServers    :: QueryPeerServers -> ProtoT QueryPeerServers st (Set Peer)
  , callQueryPeerCount      :: QueryPeerCount -> ProtoT QueryPeerCount st Int
  , callQueryLocalServices  :: QueryLocalServices -> ProtoT QueryLocalServices st [ServerRef]
}


peerServerName :: String
peerServerName = "agent:peer"

-- | Advertise a Server on the peer
registerServer :: (MonadAgent agent)
               => AgentServer -> ProcessId -> agent (Either CallFail ())
registerServer (AgentServer sname _) pid =
    castServ $ RegisterServer sname pid

-- | Get a list of Peers (from local Peer's cache)
-- which have the requested Server registered
-- Contexts and Zones
queryPeerServers :: MonadAgent agent
                => String -> Set Context -> Set Zone
                -> agent (Either CallFail (Set Peer))
queryPeerServers s c z = callServ $ QueryPeerServers s c z

-- | Returns the number of Peer Agent processes this node is connected to
queryPeerCount :: MonadAgent agent => agent (Either CallFail Int)
queryPeerCount = callServ QueryPeerCount

-- | Query a remote node string ("host:port") for its registered
-- servers, and register them locally as resolving RemoteCache would,
-- so that multiple remote whereis do not have to be sent for
-- different servers at the same node.
warmRemoteCache :: forall process. MonadProcess process
                => String -> process ()
warmRemoteCache nodestr = liftP $
 do let nodeId = makeNodeId nodestr
    pid <- getSelfPid
    nsendRemote nodeId peerServerName (QueryLocalServices, pid)
    mservers <- expectTimeout 1000000 :: Process (Maybe [ServerRef])
    case mservers of
        Nothing -> say "warmRemoteCache timed out!"
        Just servers' ->
            forM_ servers' $ \(ServerRef name' pid') ->
                register (nodestr ++ name') pid'

#define PEER_CAST(R, F)            \
instance ServerCast R where        \
   type CastProtocol R = PeerImpl ;\
   castName _ = peerServerName    ;\
   handle = F                     ;\

#define PEER_CALL(REQ, RSP, FN)             \
instance ServerCall REQ where               \
   type CallProtocol REQ = PeerImpl        ;\
   type CallResponse REQ = RSP             ;\
   callName _ = peerServerName             ;\
   respond = FN                            ;\

-- | Defined in Core.Protocol
PEER_CALL (QueryPeerServers, Set Peer, callQueryPeerServers)

data DiscoverPeers = DiscoverPeers
    deriving (Show, Typeable, Generic)
instance Binary DiscoverPeers
instance NFData DiscoverPeers where rnf = genericRnf
PEER_CAST (DiscoverPeers, castDiscoverPeers)

data RegisterPeer = RegisterPeer Peer
    deriving (Show, Typeable, Generic)
instance Binary RegisterPeer
instance NFData RegisterPeer where rnf = genericRnf
PEER_CAST (RegisterPeer, castRegisterPeer)

data RespondRegisterPeer = RespondRegisterPeer Peer
    deriving (Show, Typeable, Generic)
instance Binary RespondRegisterPeer
instance NFData RespondRegisterPeer where rnf = genericRnf
PEER_CAST (RespondRegisterPeer, castRespondRegisterPeer)

data RegisterServer = RegisterServer String ProcessId
    deriving (Show, Typeable, Generic)
instance Binary RegisterServer
instance NFData RegisterServer where rnf = genericRnf
PEER_CAST (RegisterServer, castRegisterServer)

data QueryPeerCount = QueryPeerCount
    deriving (Show, Typeable, Generic)
instance Binary QueryPeerCount
instance NFData QueryPeerCount where rnf = genericRnf
PEER_CALL (QueryPeerCount, Int, callQueryPeerCount)

data QueryLocalServices = QueryLocalServices
    deriving (Show, Typeable, Generic)
instance Binary QueryLocalServices
instance NFData QueryLocalServices where rnf = genericRnf
PEER_CALL (QueryLocalServices, [ServerRef], callQueryLocalServices)
