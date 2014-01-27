{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}

module FreeAgent.Peer where

import           AgentPrelude
import           FreeAgent.Lenses
import           Data.Binary

import           Control.Monad.State                                 (StateT, evalStateT)

import           FreeAgent.Process

data Peer = Peer { _peerUuid      :: UUID
                 , _peerProcessId :: !ProcessId
                 , _peerContexts  :: Set Context
                 , _peerZones     :: Set Zone
                 } deriving (Show, Eq, Ord, Typeable, Generic)
makeFields ''Peer
instance Binary Peer
instance NFData Peer where

data PeerState = PeerState [Peer]

data PeerCommand = TerminatePeer
    deriving (Typeable, Generic)
instance Binary PeerCommand
instance NFData PeerCommand

init :: Agent ()
init = do
    let _state = PeerState []
    evalStateT loop _state
  where
    loop = do
        command <- expect
        case command of
            TerminatePeer -> logM "Peer shutting down"
