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
import           Control.Distributed.Backend.P2P(getCapable, peerController,makeNodeId)
import           Data.UUID.V1 (nextUUID)


-- ---------------------------
-- Types
-- ---------------------------

data ServerRef = ServerRef String ProcessId
                 deriving (Show, Eq, Generic)

instance Ord ServerRef where
    ServerRef a _ `compare` ServerRef b _ = a `compare` b

instance Binary ServerRef

data Peer = Peer { _peerUuid      :: UUID
                 , _peerProcessId :: !ProcessId
                 , _peerContexts  :: Set Context
                 , _peerZones     :: Set Zone
                 , _peerServers  :: Set ServerRef
                 } deriving (Show, Eq, Typeable, Generic)
makeFields ''Peer
instance Binary Peer
instance NFData Peer where

instance Ord Peer where
    a `compare` b = (a^.uuid) `compare` (b^.uuid)


instance Addressable Peer where
    resolve peer = return $ Just $ peer^.processId

instance Addressable (Peer, AgentServer) where
    resolve (peer,server) = do --TODO: resolve (nodeid,sname)
        whereisRemoteAsync nodeid sname
        WhereIsReply _ mpid <- expect
        return mpid
      where sname = server^.name
            nodeid = processNodeId $ peer^.processId

data PeerState = PeerState Peer (Set Peer) deriving (Show)

type PeerAgent = StateT PeerState Agent

data PeerCommand = DiscoverPeers
                   | QueryPeerCount
                   | RegisterPeer Peer
                   | RespondRegisterPeer Peer
                   | RegisterServer String ProcessId
                   | QueryPeerServers String (Set Context) (Set Zone)
    deriving (Typeable, Generic)

instance Binary PeerCommand
instance NFData PeerCommand

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
            let state'' = PeerState localPeer mempty
            void $ spawnLocal $ peerController $ makeNodeId <$> (ctxt^.agentConfig.peerNodeSeeds)
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
    let self = Peer uid pid ctxts zs Set.empty
    [qdebug| Peer initialized self: #{self}|]
    return self

doDiscoverPeers :: PeerAgent ()
doDiscoverPeers = do
    pids <- liftProcess $ getCapable $ peerServer^.name
    [qdebug| DiscoverPeers found agent:peer services: #{pids} |]
    (PeerState self _) <- State.get
    forM_ pids $ \pid ->
        when ((self^.processId) /= pid) $ do
            [qdebug| Sending self: #{self} To peer: #{pid} |]
            cast pid $ RegisterPeer self

doRegisterServer :: String -> ProcessId -> PeerAgent ()
doRegisterServer sname pid =
    -- re-broadcast self when we add a new server
    State.modify addServer >> cast peerServer DiscoverPeers
  where
    addServer (PeerState self peers) =
        PeerState (self & servers .~ Set.insert sref (self^.servers)) peers
    sref = ServerRef sname pid

doRegisterPeer ::  Peer -> Bool -> PeerAgent ()
doRegisterPeer peer respond = do
    [qdebug| Received Registration for #{peer}|]
    modify peerList
    (PeerState self _) <- State.get
    when respond $ do
        [qdebug| Sending self: #{self} To peer: #{_peerProcessId self} |]
        cast (peer^.processId) (RespondRegisterPeer self)
  where peerList (PeerState self peers)
            | Set.member peer peers = PeerState self replace
            | otherwise = PeerState self $ Set.insert peer peers
          where replace = Set.insert peer (Set.delete peer peers)

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
        matchingContexts = intersectOn contexts fcontexts
        matchingZones = intersectOn zones fzones
        matchingService = intersectOn servers (Set.fromList [ServerRef fname undefined])
        intersectOn lens set' =
            Set.filter (\p -> Set.intersection (p^.lens) set' /= Set.empty) peers
