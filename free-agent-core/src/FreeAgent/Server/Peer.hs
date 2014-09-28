{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts             #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies                       #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}


module FreeAgent.Server.Peer
    ( peerServer
    )

where

import           FreeAgent.AgentPrelude
import           FreeAgent.Client.Peer
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Database.AcidState
import           FreeAgent.Process
import           FreeAgent.Server.ManagedAgent

import qualified Data.Set                        as Set

import           Control.Distributed.Backend.P2P (getCapable, makeNodeId,
                                                  peerController)
import           Control.Monad.Reader            (ask)
import           Control.Monad.State             as State (MonadState, StateT)
import           Data.UUID.V1                    (nextUUID)


data PeerPersist = PeerPersist {_persistUuid :: UUID}
     deriving (Show, Eq, Typeable, Generic)

deriveSafeStore ''PeerPersist
makeFields ''PeerPersist

data PeerState = PeerState { _stateSelf    :: Peer
                           , _stateFriends :: Set Peer
                           , _stateAcid    :: AcidState PeerPersist
                           }
makeFields ''PeerState

type PeerAgent = StateT PeerState Agent

-- ---------Acidic------------
-- Acidic Methods
-- ---------------------------

getPersist :: Query PeerPersist PeerPersist
getPersist = ask

$(makeAcidic ''PeerPersist ['getPersist])


-- -------Implementation------
-- Implementation
-- ---------------------------

peerServer :: AgentServer
peerServer =
    defineServer peerServerName
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
                  , infoHandlers =
                    [
                     agentInfoHandler $ \ (QueryLocalServices, sender :: ProcessId) ->
                           use (self.servers) >>= send sender . Set.toList
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
                  makeNodeId <$> (context' ^. agentConfig.peerNodeSeeds)
    initAcid initpp = openOrGetDb "agent-peer" initpp def
    initSelf acid' = do
        persist <- query' acid' GetPersist
        pid <- getSelfPid
        ctxts <- viewConfig contexts
        zs <- viewConfig zones
        let self' = Peer (persist ^. uuid) pid ctxts zs Set.empty
        [qdebug| Peer initialized self: #{self'}|]
        return self'

doDiscoverPeers :: PeerAgent ()
doDiscoverPeers = do
    pids <- liftProcess $ getCapable $ peerServer ^. name
    [qdebug| DiscoverPeers found agent:peer services: #{pids} |]
    self' <- use self
    forM_ pids $ \pid ->
        when ((self' ^. processId) /= pid) $ do
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
doRegisterPeer peer respond' = do
    [qdebug| Received Registration for #{peer}|]
    friends %= Set.insert peer
    self' <- use self
    when respond' $ do
        [qdebug| Sending self: #{self'} To peer: #{peerProcessId self'} |]
        cast (peer ^. processId) (RespondRegisterPeer self')

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
            Set.filter (\p -> Set.intersection (p ^. contexts) fcontexts /= Set.empty) peers
        intersectZones =
            Set.filter (\p -> Set.intersection (p ^. zones) fzones /= Set.empty) peers
        intersectService = let fservers = Set.fromList [PartialRef fname] in
            Set.filter (\p -> Set.intersection (p ^. servers) fservers /= Set.empty) peers

