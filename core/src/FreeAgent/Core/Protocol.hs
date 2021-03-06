{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- Resolvable (Target,String)

module FreeAgent.Core.Protocol
    (
      CallFail(..)
    , callTarget
    , castTarget
    , QueryPeerServers(..)
    , ServerCall(..)
    , ServerCast(..)
    , ProtoT
    , callServ
    , castServ

    ) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Process

import           Data.Binary
import qualified Data.Set                             as Set

import qualified Control.Distributed.Process.Extras   as Extras
import           Control.Error                        ((!?))

data CallFail = RoutingFailed | ServerCrash String
        deriving (Show, Eq, Typeable, Generic)

instance Binary CallFail
instance NFData CallFail where rnf = genericRnf

instance Convertible SomeException CallFail where
    safeConvert e = return $ ServerCrash (show e)

instance Convertible CallFail Text where
    safeConvert = safeConvert . show

callTarget :: (MonadAgent agent, NFSerializable a, NFSerializable b)
           => String -> a -> agent (Either CallFail b)
callTarget name' command = runExceptT $ do
    target <- viewContext targetServer
    pid <- resolve (target, name') !? RoutingFailed
    tryAny (syncCallChan pid command) >>= convExceptT

castTarget :: (MonadAgent agent, NFSerializable a)
           => String -> a -> agent (Either CallFail ())
castTarget name' command = runExceptT $ do
    target <- viewContext targetServer
    pid <- resolve (target, name') !? RoutingFailed
    cast pid command

type family ProtoT rq st a

class (NFSerializable rq
      ,NFSerializable (CallResponse rq)
      ,Show rq) => ServerCall rq where

    type CallResponse rq
    type CallResponse rq = ()
    type CallProtocol rq :: * -> *

    respond :: CallProtocol rq st -> rq -> ProtoT rq st (CallResponse rq)
    callName :: rq -> String

class (NFSerializable rq, Show rq)
      => ServerCast rq where
    type CastProtocol rq :: * -> *

    handle :: CastProtocol rq st -> rq -> ProtoT rq st ()
    castName :: rq -> String


callServ :: (ServerCall rq, MonadAgent agent)
              => rq -> agent (Either CallFail (CallResponse rq))
callServ !rq = callTarget (callName rq) rq

castServ :: (ServerCast rq, MonadAgent agent)
         => rq -> agent (Either CallFail ())
castServ !rq = castTarget (castName rq) rq

-- | We need to define some of Peer Protocol here, because the Resolvable
-- instance for (Target, String) is used in the callTarget/callServ
-- functions. Those functions also need the ServerCall / ServerCast
-- classes.
data QueryPeerServers = QueryPeerServers String (Set Context) (Set Zone)
   deriving (Show, Typeable, Generic)

instance Binary QueryPeerServers
instance NFData QueryPeerServers where rnf = genericRnf

instance Extras.Resolvable (Target, String) where
    resolve (Local, name') = Extras.resolve name'
    resolve (RemoteCache nodestr, name') = do
        mpid <- whereis $ nodestr ++ name'
        case mpid of
            Just _ -> return mpid
            Nothing ->
             do let nodeId = makeNodeId nodestr
                mpid' <- Extras.resolve (nodeId, name')
                case mpid' of
                    Just pid ->
                     do register (nodestr ++ name') pid
                        return (Just pid)
                    Nothing -> return Nothing
    resolve (Remote peer, name') = Extras.resolve (peer, name')
    resolve (Route contexts' zones', name') = do
        peers <- queryLocalPeerServers name'
                                  (Set.fromList contexts')
                                  (Set.fromList zones')
        foundPeer peers
      where foundPeer peers
                | peers == Set.empty = return Nothing
                | otherwise = let peer:_ = Set.toList peers in Extras.resolve (peer, name')

queryLocalPeerServers :: MonadProcess process
                => String -> Set Context -> Set Zone
                -> process (Set Peer)
queryLocalPeerServers s c z = syncCallChan (Local, peerServerName) $ QueryPeerServers s c z

peerServerName :: String
peerServerName = "agent:peer"
