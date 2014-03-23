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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import           FreeAgent.Lenses
import           FreeAgent.Process
import           FreeAgent.Database.AcidState
import           FreeAgent.Process.ManagedAgent

import           Data.Binary
import qualified Data.Set as Set

import qualified Control.Distributed.Process.Platform as Platform
import           Control.Monad.State                            as State (StateT, MonadState)
import           Control.Monad.Reader (ask)
import           Control.Distributed.Backend.P2P(getCapable, peerController,makeNodeId)
import           Control.Error (runEitherT, (!?), hoistEither, EitherT)
import           Control.DeepSeq.TH (deriveNFData)
import           Data.UUID.V1 (nextUUID)
import           Data.Acid.Advanced (query')


-- ---------------------------
-- Types
-- ---------------------------



instance Platform.Resolvable Peer where
    resolve peer = return $ Just $ peer^.processId

instance Platform.Resolvable (Peer, String) where
    resolve (peer,sname) = resolve (nodeid, sname)
      where nodeid = processNodeId $ peer^.processId

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

getPersist :: Query PeerPersist PeerPersist
getPersist = ask

$(makeAcidic ''PeerPersist ['getPersist])
-- ---------------------------
-- API
-- ---------------------------

-- | Advertise a Server on the local peer
registerServer :: (MonadProcess m)
               => AgentServer -> ProcessId -> m ()
registerServer s pid = cast peerServer $ RegisterServer (s^.name) pid

-- | Get a list of Peers (from local Peer's cache)
-- which have the requested Server registered
-- Contexts and Zones
queryPeerServers :: MonadProcess m
                => String -> Set Context -> Set Zone -> m (Set Peer)
queryPeerServers s c z = syncCallChan peerServer $ QueryPeerServers s c z

-- | Returns the number of Peer Agent processes this node is connected to
queryPeerCount :: MonadProcess m => m Int
queryPeerCount = syncCallChan peerServer QueryPeerCount

instance Platform.Resolvable (Target, String) where
    resolve (Local, name') = resolve name'
    resolve (Remote peer, name') = resolve (peer, name')
    resolve (Route contexts' zones', name') = do
        peers <- queryPeerServers name'
                                  (Set.fromList contexts')
                                  (Set.fromList zones')
        foundPeer peers
      where foundPeer peers
                | peers == Set.empty = return Nothing
                | otherwise = let peer:_ = Set.toList peers in resolve (peer, name')

tryAnyT :: (MonadBaseControl IO m) => m a -> EitherT SomeException m a
tryAnyT ma = lift (tryAny ma) >>= hoistEither

callServer :: (MonadAgent m, NFSerializable a, NFSerializable b)
           => String -> Target -> a -> m (Either CallFail b)
callServer name' target command = runEitherT $ do
    pid <- resolve (target, name') !? RoutingFailed
    tryAny (syncCallChan pid command) >>= convEitherT

castServer :: (MonadAgent m, NFSerializable a)
           => String -> Target -> a -> m (Either CallFail ())
castServer name' target command = runEitherT $ do
    pid <- resolve (target, name') !? RoutingFailed
    cast pid command

-- ---------------------------
-- Implementation
-- ---------------------------

peerServer :: AgentServer
peerServer =
    defineServer "agent:peer"
                 initState
                 defaultProcess {
                     apiHandlers =
                     [ handleCast $ agentCastHandler $ \ cmd ->
                         -- registration is async to avoid possiblity of deadlock
                         case cmd of
                             DiscoverPeers -> doDiscoverPeers
                             RegisterPeer peer -> doRegisterPeer peer True
                             RespondRegisterPeer peer -> doRegisterPeer peer False
                             RegisterServer name' pid -> doRegisterServer name' pid
                             _ -> $(err "illegal pattern match")

                     , handleRpcChan $ \
                           state'@( AgentState _ (PeerState _ peers _))
                           port
                           QueryPeerCount ->
                               sendChan port (length peers) >> continue state'

                     , handleRpcChan $ agentCallHandler $
                         \ (QueryPeerServers n c z) -> doQueryPeerServers n c z
                     ]
                 }

  where
    initState = do
        context' <- askContext
        Just newid <- liftIO nextUUID
        Just acid' <- initAcid (PeerPersist newid)
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
        ctxts <- viewConfig $ agentConfig.contexts
        zs <- viewConfig $ agentConfig.zones
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
    friends %= updateFriends
    self' <- use self
    when respond $ do
        [qdebug| Sending self: #{self'} To peer: #{_peerProcessId self'} |]
        cast (peer^.processId) (RespondRegisterPeer self')
  where updateFriends peers
            | Set.member peer peers = Set.insert peer (Set.delete peer peers)
            | otherwise = Set.insert peer peers

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
