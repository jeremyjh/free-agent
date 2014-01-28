{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
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

module FreeAgent.Peer where

import           AgentPrelude
import           FreeAgent.Lenses
import           FreeAgent.Core
import           FreeAgent.Process
import qualified FreeAgent.Executive as Exec (execServer)
import           FreeAgent.Process.ManagedAgent

import           Data.Binary

import           Control.Distributed.Process.Platform.Supervisor
import           Control.Distributed.Process.Platform.Time
import           Control.Monad.State                            (StateT )


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

data PeerState = PeerState [Peer]

type PeerAgent = StateT PeerState Agent

data PeerCommand = DiscoverPeers | QueryPeerCount
    deriving (Typeable, Generic)

instance Binary PeerCommand
instance NFData PeerCommand

-- ---------------------------
-- API
-- ---------------------------

startPeer :: AgentContext -> Process ()
startPeer ctxt = void $ withAgent ctxt $ startServer peerServer


-- ---------------------------
-- Implementation
-- ---------------------------

peerServer :: AgentServer
peerServer = AgentServer "agent:peer" init child
  where
    init ctxt = do
        let state' = PeerState []
        serve state' (initState ctxt) peerProcess
      where
        peerProcess = defaultProcess {
            apiHandlers =
            [ handleCast $ agentCastHandler $ \ DiscoverPeers -> doDiscoverPeers

            , handleRpcChan $ \
                state'@(AgentState _ (PeerState xs)) port QueryPeerCount ->
                    sendChan port (length xs) >> continue state'
            ]

        }

    child ctxt = do
       initChild <- toChildStart $ init ctxt

       return $ ChildSpec {
             childKey     = ""
           , childType    = Worker
           , childRestart = Permanent
           , childStop    = TerminateTimeout (Delay $ milliSeconds 10)
           , childStart   = initChild
           , childRegName = Just $ LocalName "agent:peer"
       }

-- because for some reason, runhaskell panics when I have a
-- {-# SOURCE -#} Executive in Core.hs
execServer :: AgentServer
execServer = Exec.execServer

doDiscoverPeers :: PeerAgent ()
doDiscoverPeers = undefined
