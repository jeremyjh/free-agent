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
    )

where

import           AgentPrelude
import           FreeAgent.Lenses
import           FreeAgent.Process
import           FreeAgent.Core
import           FreeAgent.Database.AcidState
import           FreeAgent.Process.ManagedAgent

import           Data.Binary
import qualified Data.Set as Set

import           Control.Monad.State                            as State (StateT, MonadState)
import Control.Monad.Reader (ask)
import           Control.Distributed.Backend.P2P(getCapable, peerController,makeNodeId)
import           Data.UUID.V1 (nextUUID)
import           Data.Acid.Advanced (query')


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

{-deleteAction :: Key -> Update PersistExec ()-}
{-deleteAction key' =-}
    {-actions %= Map.delete key'-}

getPersist :: Query PeerPersist PeerPersist
getPersist = ask

$(makeAcidic ''PeerPersist ['getPersist])
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
    init ctxt = serve undefined initPeer peerProcess
      where
        initPeer _ = do
            initp2p
            Just newid <- liftIO nextUUID
            Just acid' <- initAcid (PeerPersist newid)
            self' <- initSelf acid'
            let state' = PeerState self' (Set.fromList [self']) acid'
            getSelfPid >>= flip cast DiscoverPeers
            return $ InitOk (AgentState ctxt state') Infinity
          where
            initp2p = void $ spawnLocal $ peerController $
                        makeNodeId <$> (ctxt^.agentConfig.peerNodeSeeds)
            initAcid initpp = withAgent ctxt $ openOrGetDb "agent-peer" initpp def
            initSelf acid' =
                withAgent ctxt $ do
                    persist <- query' acid' GetPersist
                    pid <- getSelfPid
                    ctxts <- viewConfig $ agentConfig.contexts
                    zs <- viewConfig $ agentConfig.zones
                    let self' = Peer (persist^.uuid) pid ctxts zs Set.empty
                    [qdebug| Peer initialized self: #{self'}|]
                    return self'
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
                  state'@( AgentState _ (PeerState _ peers _))
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
