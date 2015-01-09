{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies                       #-}


module FreeAgent.Server.ManagedAgent
    ( module Managed
    , module Supervisor
    , ServerCall(..)
    , ServerCast(..)
    , registerCall
    , registerCallAsync
    , registerCast
    , AgentState(..)
    , defineServer
    , callServ
    , castServ
    , callTarget
    , castTarget
    , runLogEitherT
    , agentCastHandler
    , agentRpcHandler
    , agentRpcAsyncHandler
    , agentInfoHandler
    , agentExitHandler
    , Addressable
    , Delay(..)
    , ChildSpec(..)
    , milliSeconds
    )

where

import FreeAgent.AgentPrelude
import FreeAgent.Core.Agent                                (spawnAgent, withAgent)
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Core.Protocol
import FreeAgent.Process

import Control.Monad.State                                 (StateT, evalStateT,
                                                            execStateT, runStateT)

import qualified Data.Map.Strict as Map



import Control.Concurrent.Lifted                           (threadDelay)
import Control.Distributed.Process.Platform                (Addressable, linkOnFailure)
import Control.Distributed.Process.Platform.ManagedProcess as Managed hiding (action,
                                                                       call, cast,
                                                                       shutdown,
                                                                       syncCallChan)
import Control.Distributed.Process.Platform.Supervisor     as Supervisor
import Control.Distributed.Process.Platform.Time           (Delay (..), milliSeconds)

type AgentStats = Map Text Int

data AgentState a = AgentState !AgentStats !AgentContext !a

registerCall :: forall st rq.
                ( ServerCall rq
                , ProtoT rq st (CallResponse rq) ~ StateT st Agent (CallResponse rq))
             => CallProtocol rq st
             -> Proxy rq -> Dispatcher (AgentState st)
registerCall impl _ = agentRpcHandler
    (respond impl :: rq -> ProtoT rq st (CallResponse rq))

-- | Like registerCall, but spawns a new process which will reply when
-- complete to the caller. *Warning*: This mean changes to state are discarded.
registerCallAsync :: forall st rq.
                     ( ServerCall rq
                     , ProtoT rq st (CallResponse rq) ~ StateT st Agent (CallResponse rq))
                  => CallProtocol rq st
                  -> Proxy rq -> Dispatcher (AgentState st)
registerCallAsync impl _ = agentRpcAsyncHandler
    (respond impl :: rq -> ProtoT rq st (CallResponse rq))

registerCast :: forall st rq.
                ( ServerCast rq
                , ProtoT rq st () ~ StateT st Agent ())
             => CastProtocol rq st
             -> Proxy rq -> Dispatcher (AgentState st)
registerCast impl _ = agentCastHandler (handle impl :: rq -> ProtoT rq st ())

defineServer :: String -- ^ server name
             -> Agent st -- ^ state intialization
             -> ProcessDefinition (AgentState st)
             -> AgentServer
defineServer name' initState' processDef' = AgentServer name' child
  where
    init context' = do
        state' <- withAgent context' initState'
        serve state' onStart processDef'
      where onStart state' =
                return $ InitOk (AgentState mempty context' state') Infinity

    child ctxt = do
        initChild <- toChildStart $ init ctxt
        return ChildSpec {
              childKey     = name'
            , childType    = Worker
            , childRestart = Permanent
            , childStop    = TerminateTimeout (Delay $ milliSeconds 5000)
            , childStart   = initChild
            , childRegName = Just $ LocalName name'
        }

incrStats :: Typeable rq => AgentState st -> rq -> AgentState st
incrStats (AgentState stats c u) rq =
    let updated = Map.insertWith (+) (typeName rq) 1 stats
    in AgentState updated c u

-- | handle channel RPC calls that may mutate the State environment
agentRpcHandler :: (NFSerializable a, NFSerializable b, Show a)
                => (a -> (StateT s Agent) b)
                -> Dispatcher (AgentState s)
agentRpcHandler fn =
    handleRpcChan $ \ state' replyCh command ->
        let ma = debugLog command >> fn command
            state'' = incrStats state' command
        in runAgentStateT state'' (sendChan replyCh) ma

-- | handle RPC calls that do not mutate state out of band in stateful process
agentRpcAsyncHandler :: (NFSerializable a, NFSerializable b, Show a)
                     => (a -> (StateT s Agent) b)
                     -> Dispatcher (AgentState s)
agentRpcAsyncHandler fn =
    handleRpcChan $ \ state' replyCh command ->
        let ma = debugLog command >> fn command
            state'' = incrStats state' command
        in runAgentStateTAsync state'' (sendChan replyCh) ma

-- | wrapper for commands that will handle a cast and mutate state -
-- the updated StateT environment will provide the state value to
-- 'continue'
agentCastHandler :: (NFSerializable a, Show a)
                 => (a -> (StateT s Agent) ())
                 -> Dispatcher (AgentState s)
agentCastHandler fn =
    handleCast $ \ state' command ->
        let ma = debugLog command >> fn command
            state'' = incrStats state' command
        in runAgentStateT state'' (const $ return ()) ma

-- | wrapper for commands that will handle an info message
-- the updated StateT environment will provide the state value to
-- 'continue'
agentInfoHandler :: (NFSerializable a, Show a)
                 => (a -> (StateT s Agent) ())
                 -> DeferredDispatcher (AgentState s)
agentInfoHandler fn =
    handleInfo $ \ state' command ->
        let ma = debugLog command >> fn command
            state'' = incrStats state' command
        in runAgentStateT state'' (const $ return ()) ma

runAgentStateT :: (NFSerializable a)
               => AgentState s
               -> (a -> Agent ())
               -> StateT s Agent a
               -> Process (ProcessAction (AgentState s) )
runAgentStateT (AgentState stats ctxt ustate) respond' ma = do
    newState <- withAgent ctxt $ do
        (result, newState) <- runStateT ma ustate
        respond' result
        return newState
    continue $ AgentState stats ctxt newState

runAgentStateTAsync :: (NFSerializable a)
                    => AgentState s
                    -> (a -> Agent ())
                    -> StateT s Agent a
                    -> Process (ProcessAction (AgentState s) )
runAgentStateTAsync state'@(AgentState _ ctxt ustate) respond' ma = do
    pid <- spawnAgent ctxt $ do
        threadDelay 1000 -- so we have time to setup a link in parent
        evalStateT ma ustate >>= respond'
    -- TODO: This will prevent the client call from hanging forever
    -- on an exception/crash but it would be better for executive to monitor child process
    -- and send an exit to the sendPortProcessId
    liftP $ linkOnFailure pid
    continue state'

runLogEitherT :: (Show msg, Show e, MonadLogger m)
              => msg -> EitherT e m a -> m (Either e a)
runLogEitherT msg ma =
    runEitherT ma >>= logEitherT
  where logEitherT left'@(Left reason) = do
            [qwarn| Processing for #{msg} failed with reason: #{reason} |]
            return left'
        logEitherT right' = return right'

agentExitHandler :: (NFSerializable a, Show a)
                 => (ProcessId -> a -> (StateT s Agent) ())
                 -> AgentState s -> ProcessId -> a
                 -> Process (ProcessAction (AgentState s) )
agentExitHandler fn (AgentState stats ctxt ustate) pid reason = do
    newState <- withAgent ctxt $ do
        [qdebug| Handling exit reason #{reason}|]
        execStateT (fn pid reason) ustate
    continue $ AgentState stats ctxt newState

debugLog :: (Show a, MonadLogger m) => a -> m ()
debugLog c = [qdebug| Processing command: #{c}|]
