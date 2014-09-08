{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, ScopedTypeVariables #-}

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
    , warmRemoteCache
    ) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Process

import           Data.Binary
import qualified Data.Set                             as Set

import           Control.Distributed.Backend.P2P      (makeNodeId)
import qualified Control.Distributed.Process.Platform as Platform

import           Control.Error                        ((!?))

data CallFail = RoutingFailed | ServerCrash String
        deriving (Show, Eq, Typeable, Generic)

instance Binary CallFail
instance Convertible SomeException CallFail where
    safeConvert e = return $ ServerCrash (show e)

data PeerCommand = DiscoverPeers
                   | QueryPeerCount
                   | QueryLocalServices
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

instance Platform.Resolvable (Target, String) where
    resolve (Local, name') = resolve name'
    resolve (RemoteCache nodestr, name') = do
        mpid <- whereis $ nodestr ++ name'
        case mpid of
            Just _ -> return mpid
            Nothing ->
             do let nodeId = makeNodeId nodestr
                mpid' <- resolve (nodeId, name')
                case mpid' of
                    Just pid ->
                     do register (nodestr ++ name') pid
                        return (Just pid)
                    Nothing -> return Nothing
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
