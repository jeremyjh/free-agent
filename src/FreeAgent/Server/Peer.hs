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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Server.Peer
    ( peerServer
    , registerServer
    , queryPeerServers
    , queryPeerCount
    )

where

import           AgentPrelude
import           FreeAgent.Lenses
import           FreeAgent.Process
import           FreeAgent.Core
import           FreeAgent.Database as DB
import qualified FreeAgent.Database.KeySpace as KS
import           FreeAgent.Process.ManagedAgent

import           Data.Binary
import qualified Data.Set as Set

import           Control.Monad.State                            as State (StateT, MonadState)
import           Control.Distributed.Backend.P2P(getCapable, peerController,makeNodeId)
import           Data.UUID.V1 (nextUUID)


-- ---------------------------
-- Types
-- ---------------------------



instance Addressable Peer where
    resolve peer = return $ Just $ peer^.processId

instance Addressable (Peer, AgentServer) where
    resolve (peer,server) = do --TODO: resolve (nodeid,sname)
        whereisRemoteAsync nodeid sname
        WhereIsReply _ mpid <- expect
        return mpid
      where sname = server^.name
            nodeid = processNodeId $ peer^.processId

declareLenses [d|
    data PeerState = PeerState { self :: Peer
                               , friends :: Set Peer
                               } deriving (Show)
              |]

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

-- ---------------------------
-- API
-- ---------------------------

{-discoverPeers :: (MonadProcess m) => m ()-}
-- | Advertise a Server on the local peer
registerServer :: (MonadProcess m)
               => AgentServer -> ProcessId -> m ()
registerServer s pid = cast peerServer $ RegisterServer (s^.name) pid

-- | Get a list of Peers (from local Peer's cache)
-- which have the requested Server registered
-- Contexts and Zones
queryPeerServers :: MonadProcess m
                => AgentServer -> Set Context -> Set Zone -> m (Set Peer)
queryPeerServers s c z = syncCallChan peerServer $ QueryPeerServers (s^.name) c z

-- | Returns the number of Peer Agent processes this node is connected to
queryPeerCount :: MonadProcess m => m Int
queryPeerCount = syncCallChan peerServer QueryPeerCount


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
            let state'' = PeerState localPeer (Set.fromList [localPeer])
            void $ spawnLocal $ peerController $ makeNodeId <$> (ctxt^.agentConfig.peerNodeSeeds)
            pid <- getSelfPid; cast pid DiscoverPeers
            return $ InitOk (AgentState ctxt state'') Infinity
        peerProcess = defaultProcess {
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
    let self' = Peer uid pid ctxts zs Set.empty
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
    matchingAll peers = Set.intersection (Set.intersection matchingContexts
                                                           matchingZones)
                                         matchingService
      where
        matchingContexts = intersectOn contexts fcontexts
        matchingZones = intersectOn zones fzones
        matchingService = intersectOn servers (Set.fromList [ServerRef fname undefined])
        intersectOn lens set' =
            Set.filter (\p -> Set.intersection (p^.lens) set' /= Set.empty) peers
