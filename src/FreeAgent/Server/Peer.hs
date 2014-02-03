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

module FreeAgent.Server.Peer where

import           AgentPrelude
import           FreeAgent.Lenses
import           FreeAgent.Process
import           FreeAgent.Core
import           FreeAgent.Database as DB
import qualified FreeAgent.Database.KeySpace as KS
import           FreeAgent.Process.ManagedAgent

import           Data.Binary
import qualified Data.Set as Set

import           Control.Monad.State                            as State (StateT, modify, get, MonadState)
import           Control.Distributed.Backend.P2P(getCapable, startPeerController,makeNodeId)
import           Data.UUID.V1 (nextUUID)


-- ---------------------------
-- Types
-- ---------------------------

data Peer = Peer { _peerUuid      :: UUID
                 , _peerProcessId :: !ProcessId
                 , _peerContexts  :: Set Context
                 , _peerZones     :: Set Zone
                 , _peerServers  :: Set String
                 } deriving (Show, Eq, Typeable, Generic)
makeFields ''Peer
instance Binary Peer
instance NFData Peer where

instance Ord Peer where
    a `compare` b = (a^.uuid) `compare` (b^.uuid)

data PeerState = PeerState Peer (Set Peer) deriving (Show)

type PeerAgent = StateT PeerState Agent

data PeerCommand = DiscoverPeers
                   | QueryPeerCount
                   | RegisterPeer Peer
                   | RespondRegisterPeer Peer
                   | RegisterServer String
                   | QueryPeerServers String (Set Context) (Set Zone)
    deriving (Typeable, Generic)

instance Binary PeerCommand
instance NFData PeerCommand

-- ---------------------------
-- API
-- ---------------------------

-- | Advertise a Server on the local peer
registerServer :: (MonadProcess m) => AgentServer -> m ()
registerServer s =  cast peerServer $ RegisterServer (s^.name)

-- | Get a list of Peers (from local Peer's cache)
-- which have the requested Server registered
-- Contexts and Zones
queryPeerServers :: MonadAgent m
                => AgentServer -> Set Context -> Set Zone -> m (Set Peer)
queryPeerServers s c z = syncCallChan peerServer $ QueryPeerServers (s^.name) c z

-- ---------------------------
-- Implementation
-- ---------------------------

peerServer :: AgentServer
peerServer = AgentServer sname init child
  where
    sname = "agent:peer"
    init ctxt = do
        let state' = PeerState undefined mempty
        serve state' initPeer peerProcess
      where
        initPeer _ = do
            localPeer <- withAgent ctxt initSelf
            let state'' = PeerState localPeer mempty
            startPeerController $ makeNodeId <$> (ctxt^.agentConfig.peerNodeSeeds)
            return $ InitOk (AgentState ctxt state'') Infinity
        peerProcess = defaultProcess {
            apiHandlers =
            [ handleCast $ agentCastHandler $ \ cmd ->
                -- registration is async to avoid possiblity of deadlock
                case cmd of
                    DiscoverPeers -> doDiscoverPeers
                    RegisterPeer peer -> doRegisterPeer peer True
                    RespondRegisterPeer peer -> doRegisterPeer peer False
                    RegisterServer name' -> doRegisterServer name'
                    _ -> $(err "illegal pattern match")

            , handleRpcChan $ \
                  state'@( AgentState _ (PeerState _ peers))
                  port
                  QueryPeerCount ->
                      sendChan port (length peers) >> continue state'

            , handleRpcChan $ agentCallHandler $
                \ (QueryPeerServers n c z) -> doQueryPeerServers n c z
            ]

        }

    child ctxt = do
       initChild <- toChildStart $ init ctxt

       return ChildSpec {
             childKey     = ""
           , childType    = Worker
           , childRestart = Permanent
           , childStop    = TerminateTimeout (Delay $ milliSeconds 10)
           , childStart   = initChild
           , childRegName = Just $ LocalName sname
       }

initSelf :: Agent Peer
initSelf = do
    muid <- agentDb $ withKeySpace KS.peer $
                         DB.get "uuid"
    uid <- case muid of
       Just u -> return $ fromBytes u
       Nothing -> do
            Just newid <- liftIO nextUUID
            [qdebug| Initializing new Peer UUID #{newid}|]
            agentDb $ withKeySpace KS.peer $
                DB.put "uuid" (toBytes newid)
            return newid
    pid <- getSelfPid
    ctxts <- viewConfig $ agentConfig.contexts
    zs <- viewConfig $ agentConfig.zones
    let self = Peer uid pid ctxts zs Set.empty
    [qdebug| Peer initialized self: #{self}|]
    return self

doDiscoverPeers :: PeerAgent ()
doDiscoverPeers = do
    seeds <- viewConfig $ agentConfig.peerNodeSeeds
    [qdebug| Attempting to connect to peer seeds: #{seeds} |]
    pids <- liftProcess $ getCapable $ peerServer^.name
    [qdebug| Found agent:peer services: #{pids} |]
    (PeerState self _) <- State.get
    forM_ pids $ \pid ->
        when ((self^.processId) /= pid) $ do
            [qdebug| Sending self: #{self} To peer: #{pid} |]
            cast pid $ RegisterPeer self

doRegisterServer :: (MonadState PeerState m) => String -> m ()
doRegisterServer sname = State.modify addServer
  where
    addServer (PeerState self peers) =
        PeerState (self & servers .~ (Set.insert sname (self^.servers))) peers

doRegisterPeer ::  Peer -> Bool -> PeerAgent ()
doRegisterPeer peer respond = do
    [qdebug| Received Registration for #{peer}|]
    modify peerList
    (PeerState self _) <- State.get
    when respond $ do
        [qdebug| Sending self: #{self} To peer: #{_peerProcessId self} |]
        cast (peer^.processId) (RespondRegisterPeer self)
  where peerList (PeerState self peers)
            | Set.member peer peers = PeerState self peers
            | otherwise = PeerState self $ Set.insert peer peers

doQueryPeerServers :: MonadState PeerState m
                  => String -> Set Context -> Set Zone -> m (Set Peer)
doQueryPeerServers fname fcontexts fzones = do
    (PeerState _ peers) <- State.get
    return $ matchingAll peers
  where
    matchingAll peers = Set.intersection (Set.intersection matchingContexts
                                                           matchingZones)
                                         matchingService
      where
        matchingContexts = intersectOn contexts (debug fcontexts)
        matchingZones = intersectOn zones fzones
        matchingService = intersectOn servers (Set.fromList [fname])
        intersectOn lens set' =
            Set.filter (\p -> Set.intersection (p^.lens) set' /= Set.empty) peers
