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
import           FreeAgent.Database as DB
import qualified FreeAgent.Database.KeySpace as KS
import           FreeAgent.Process.ManagedAgent

import           Data.Binary

import           Control.Distributed.Process.Platform.Supervisor
import           Control.Distributed.Process.Platform.Time
import           Control.Monad.State                            as State (StateT, put, get)
import           Control.Distributed.Backend.P2P(getCapable, startPeerController,makeNodeId)
import           Data.UUID.V1 (nextUUID)



-- ---------------------------
-- Types
-- ---------------------------

data Peer = Peer { _peerUuid      :: UUID
                 , _peerProcessId :: !ProcessId
                 , _peerContexts  :: Set Context
                 , _peerZones     :: Set Zone
                 } deriving (Show, Eq, Ord, Typeable, Generic)
makeFields ''Peer
instance Binary Peer
instance NFData Peer where

data PeerState = PeerState [Peer] deriving (Show)

type PeerAgent = StateT PeerState Agent

data PeerCommand = DiscoverPeers | QueryPeerCount | RegisterPeer Peer | RespondRegisterPeer Peer
    deriving (Typeable, Generic)

instance Binary PeerCommand
instance NFData PeerCommand

-- ---------------------------
-- API
-- ---------------------------



-- ---------------------------
-- Implementation
-- ---------------------------

peerServer :: AgentServer
peerServer = AgentServer "agent:peer" init child
  where
    init ctxt = do
        let state' = PeerState []
        serve state' initPeer peerProcess
      where
        initPeer state' = do
            startPeerController $ makeNodeId <$> (ctxt^.agentConfig.peerNodeSeeds)
            return $ InitOk (AgentState ctxt state') Infinity
        peerProcess = defaultProcess {
            apiHandlers =
            [ handleCast $ agentCastHandler $ \ cmd ->
                -- registration is async to avoid possiblity of deadlock
                case cmd of
                    DiscoverPeers -> doDiscoverPeers
                    RegisterPeer peer -> doRegisterPeer peer True
                    RespondRegisterPeer peer -> doRegisterPeer peer False
                    _ -> $(err "illegal pattern match")

            , handleRpcChan $ \
                  state'@( AgentState _ (PeerState xs))
                  port QueryPeerCount ->
                      sendChan port (length xs) >> continue state'
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
           , childRegName = Just $ LocalName "agent:peer"
       }



localPeer :: PeerAgent Peer
localPeer = do
    muid <- agentDb . withKeySpace KS.peer $
                         DB.get "uuid"
    uid <- case muid of
       Just u -> return $ fromBytes u
       Nothing -> do
            Just newid <- liftIO nextUUID
            agentDb . withKeySpace KS.peer $
                DB.put "uuid" (toBytes newid)
            return newid
    pid <- getSelfPid
    ctxts <- viewConfig $ agentConfig.contexts
    zs <- viewConfig $ agentConfig.zones
    return $ Peer uid pid ctxts zs


doDiscoverPeers :: PeerAgent ()
doDiscoverPeers = do
    seeds <- viewConfig $ agentConfig.peerNodeSeeds
    [qdebug| Attempting to connect to peer seeds: #{seeds} |]
    pids <- liftProcess . getCapable $ peerServer^.name
    [qdebug| Found agent:peer services: #{pids} |]
    self <- localPeer
    forM_ pids $ \pid ->
        when ((self^.processId) /= pid) $ do
            [qdebug| Sending self: #{self} to peer: #{pid} |]
            cast pid $ RegisterPeer self

doRegisterPeer ::  Peer -> Bool -> PeerAgent ()
doRegisterPeer peer respond = do
    [qdebug| Received Registration for #{peer}|]
    (PeerState peers) <- State.get
    State.put $ PeerState (peer : peers)
    self <- localPeer
    when respond $ cast (peer^.processId) (RespondRegisterPeer self)
