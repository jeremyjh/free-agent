{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- Resolvable (Target,String)

module FreeAgent.Server.Peer
    ( peerServer
    , registerServer
    , queryPeerServers
    , queryPeerCount
    , callServer
    , castServer
    , CallFail(..)
    )

where

import           AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Process
import           FreeAgent.Database.AcidState
import           FreeAgent.Server.ManagedAgent

import           Data.Binary
import qualified Data.Set as Set

import qualified Control.Distributed.Process.Platform as Platform
import           Control.Monad.State                            as State (StateT, MonadState)
import           Control.Monad.Reader (ask)
import           Control.Distributed.Backend.P2P(getCapable, peerController,makeNodeId)
import           Control.Error ((!?), hoistEither)
import           Control.DeepSeq.TH (deriveNFData)
import           Data.UUID.V1 (nextUUID)

-- ---------Types-------------
-- Types
-- ---------------------------

data CallFail = RoutingFailed | ServerCrash String
        deriving (Show, Eq, Typeable, Generic)
instance Binary CallFail

instance Convertible SomeException CallFail where
    safeConvert e = return $ ServerCrash (show e)

data PeerPersist = PeerPersist {_persistUuid :: UUID}
     deriving (Show, Eq, Typeable, Generic)

deriveSafeStore ''PeerPersist
makeFields ''PeerPersist

data PeerState = PeerState { _stateSelf :: Peer
                           , _stateFriends :: Set Peer
                           , _stateAcid :: AcidState PeerPersist
                           }
makeFields ''PeerState

type PeerAgent = StateT PeerState Agent

data PeerCommand = DiscoverPeers
                   | QueryPeerCount
                   | RegisterPeer Peer
                   | RespondRegisterPeer Peer
                   | RegisterServer String ProcessId
                   | QueryPeerServers String (Set Context) (Set Zone)
    deriving (Show, Typeable, Generic)

instance Binary PeerCommand
instance NFData PeerCommand


-- ---------Acidic------------
-- Acidic Methods
-- ---------------------------

getPersist :: Query PeerPersist PeerPersist
getPersist = ask

$(makeAcidic ''PeerPersist ['getPersist])

-- ---------API---------------
-- API
-- ---------------------------

-- | Advertise a Server on the peer
registerServer :: (MonadAgent agent)
               => AgentServer -> ProcessId -> agent (Either CallFail ())
registerServer (AgentServer sname _) pid =
    castServer serverName $ RegisterServer sname pid

-- | Get a list of Peers (from local Peer's cache)
-- which have the requested Server registered
-- Contexts and Zones
queryPeerServers :: MonadAgent agent
                => String -> Set Context -> Set Zone
                -> agent (Either CallFail (Set Peer))
queryPeerServers s c z = callServer serverName $ QueryPeerServers s c z

-- | Returns the number of Peer Agent processes this node is connected to
queryPeerCount :: MonadAgent agent => agent (Either CallFail Int)
queryPeerCount = callServer serverName QueryPeerCount

instance Platform.Resolvable (Target, String) where
    resolve (Local, name') = resolve name'
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
queryLocalPeerServers s c z = syncCallChan peerServer $ QueryPeerServers s c z

tryAnyT :: (MonadBaseControl IO m) => m a -> EitherT SomeException m a
tryAnyT ma = lift (tryAny ma) >>= hoistEither

callServer :: (MonadAgent agent, NFSerializable a, NFSerializable b)
           => String -> a -> agent (Either CallFail b)
callServer name' command = runEitherT $ do
    target <- viewContext targetServer
    pid <- resolve (target, name') !? RoutingFailed
    tryAny (syncCallChan pid command) >>= convEitherT

castServer :: (MonadAgent agent, NFSerializable a)
           => String -> a -> agent (Either CallFail ())
castServer name' command = runEitherT $ do
    target <- viewContext targetServer
    pid <- resolve (target, name') !? RoutingFailed
    cast pid command

-- -------Implementation------
-- Implementation
-- ---------------------------

serverName :: String
serverName = "agent:peer"

peerServer :: AgentServer
peerServer =
    defineServer serverName
                 initState
                 defaultProcess {
                     apiHandlers =
                     [ agentCastHandler $ \ cmd ->
                         -- registration is async to avoid possiblity of deadlock
                         case cmd of
                             DiscoverPeers -> doDiscoverPeers
                             RegisterPeer peer -> doRegisterPeer peer True
                             RespondRegisterPeer peer -> doRegisterPeer peer False
                             RegisterServer name' pid -> doRegisterServer name' pid
                             _ -> $(err "illegal pattern match")

                     , agentRpcHandler $ \ QueryPeerCount -> uses friends length

                     , agentRpcHandler $ \ (QueryPeerServers n c z) ->
                           doQueryPeerServers n c z
                     ]
                 }

  where
    initState = do
        context' <- askContext
        Just newid <- liftIO nextUUID
        acid' <- initAcid (PeerPersist newid)
        self' <- initSelf acid'
        liftProcess $ initp2p context' >>= link
        getSelfPid >>= flip cast DiscoverPeers
        return $ PeerState self' (Set.fromList [self']) acid'
    initp2p context' = spawnLocal $ peerController $
                  makeNodeId <$> (context'^.agentConfig.peerNodeSeeds)
    initAcid initpp = openOrGetDb "agent-peer" initpp def
    initSelf acid' = do
        persist <- query' acid' GetPersist
        pid <- getSelfPid
        ctxts <- viewConfig contexts
        zs <- viewConfig zones
        let self' = Peer (persist^.uuid) pid ctxts zs Set.empty
        [qdebug| Peer initialized self: #{self'}|]
        return self'

doDiscoverPeers :: PeerAgent ()
doDiscoverPeers = do
    pids <- liftProcess $ getCapable $ peerServer^.name
    [qdebug| DiscoverPeers found agent:peer services: #{pids} |]
    self' <- use self
    forM_ pids $ \pid ->
        when ((self'^.processId) /= pid) $ do
            [qdebug| Sending self: #{self'} To peer: #{pid} |]
            cast pid $ RegisterPeer self'

doRegisterServer :: String -> ProcessId -> PeerAgent ()
doRegisterServer sname pid = do
    [qdebug| Registering local AgentServer #{sname}|]
    self.servers %= Set.insert sref
    use self >>= flip doRegisterPeer False
    -- re-broadcast self when we add a new server
    cast peerServer DiscoverPeers
  where sref = ServerRef sname pid

doRegisterPeer ::  Peer -> Bool -> PeerAgent ()
doRegisterPeer peer respond = do
    [qdebug| Received Registration for #{peer}|]
    friends %= Set.insert peer
    self' <- use self
    when respond $ do
        [qdebug| Sending self: #{self'} To peer: #{peerProcessId self'} |]
        cast (peer^.processId) (RespondRegisterPeer self')

doQueryPeerServers :: MonadState PeerState m
                  => String -> Set Context -> Set Zone -> m (Set Peer)
doQueryPeerServers fname fcontexts fzones = do
    peers <- use friends
    return $ matchingAll peers
  where
    matchingAll peers = Set.intersection (Set.intersection intersectContexts
                                                           intersectZones)
                                         intersectService
      where
        intersectContexts =
            Set.filter (\p -> Set.intersection (p^.contexts) fcontexts /= Set.empty) peers
        intersectZones =
            Set.filter (\p -> Set.intersection (p^.zones) fzones /= Set.empty) peers
        intersectService = let fservers = Set.fromList [ServerRef fname undefined] in
            Set.filter (\p -> Set.intersection (p^.servers) fservers /= Set.empty) peers


deriveNFData ''CallFail
