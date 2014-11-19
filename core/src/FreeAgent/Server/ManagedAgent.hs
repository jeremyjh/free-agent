{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies                 #-}


module FreeAgent.Server.ManagedAgent
    ( module Managed
    , module Supervisor
    , ServerCall(..)
    , ServerCast(..)
    , registerCall
    , registerCast
    , AgentState(..)
    , defineServer
    , CallFail(..)
    , callServ
    , castServ
    , callTarget
    , castTarget
    , runLogEitherT
    , agentCastHandler
    , agentCastHandlerET
    , agentRpcHandler
    , agentRpcHandlerET
    , agentRpcAsyncHandler
    , agentRpcAsyncHandlerET
    , agentInfoHandler
    , agentExitHandler
    , serverState
    , Addressable
    , Delay(..)
    , ChildSpec(..)
    , milliSeconds
    )

where

import FreeAgent.AgentPrelude
import FreeAgent.Client.Peer                               (CallFail (..),
                                                            callTarget,
                                                            castTarget)
import FreeAgent.Core                                      (spawnAgent,
                                                            withAgent)
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Process

import Control.Monad.State                                 (StateT, evalStateT,
                                                            execStateT,
                                                            runStateT)


import Control.Concurrent.Lifted                           (threadDelay)
import Control.Distributed.Process.Platform                (Addressable,
                                                            linkOnFailure)
import Control.Distributed.Process.Platform.ManagedProcess as Managed hiding
                                                                       (action,
                                                                       call,
                                                                       cast,
                                                                       shutdown, syncCallChan)
import Control.Distributed.Process.Platform.Supervisor     as Supervisor
import Control.Distributed.Process.Platform.Time           (Delay (..),
                                                            milliSeconds)

data AgentState a = AgentState AgentContext a

class (NFSerializable rq
      ,NFSerializable (CallResponse rq)
      ,Show rq) => ServerCall rq where

    type CallResponse rq
    type CallResponse rq = ()
    type CallProtocol rq :: * -> *

    respond :: CallProtocol rq st -> rq -> StateT st Agent (CallResponse rq)
    callName :: rq -> String

class (NFSerializable rq, Show rq)
      => ServerCast rq where
    type CastProtocol rq :: * -> *

    handle :: CastProtocol rq st -> rq -> StateT st Agent ()
    castName :: rq -> String


registerCall :: forall st rq. (ServerCall rq)
             => CallProtocol rq st
             -> Proxy rq -> Dispatcher (AgentState st)
registerCall impl _ = agentRpcHandler
    (respond impl :: rq -> StateT st Agent (CallResponse rq))

registerCast :: forall st rq. (ServerCast rq)
             => CastProtocol rq st
             -> Proxy rq -> Dispatcher (AgentState st)
registerCast impl _ = agentCastHandler (handle impl :: rq -> StateT st Agent ())

callServ :: (ServerCall rq, MonadAgent agent)
              => rq -> agent (Either CallFail (CallResponse rq))
callServ !rq = callTarget (callName rq) rq

castServ :: (ServerCast rq, MonadAgent agent)
         => rq -> agent (Either CallFail ())
castServ !rq = castTarget (castName rq) rq

serverState :: Lens' (AgentState a) a
serverState f (AgentState c a) = fmap (AgentState c) (f a)

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
                return $ InitOk (AgentState context' state') Infinity

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

-- | handle channel RPC calls that may mutate the State environment
agentRpcHandler :: (NFSerializable a, NFSerializable b, Show a)
                => (a -> (StateT s Agent) b)
                -> Dispatcher (AgentState s)
agentRpcHandler fn =
    handleRpcChan $ \ state' replyCh command ->
        let ma = debugLog command >> fn command
        in runAgentStateT state' (sendChan replyCh) ma

-- | handle RPC calls that do not mutate state out of band in stateful process
agentRpcAsyncHandler :: (NFSerializable a, NFSerializable b, Show a)
                     => (a -> (StateT s Agent) b)
                     -> Dispatcher (AgentState s)
agentRpcAsyncHandler fn =
    handleRpcChan $ \ state' replyCh command ->
        let ma = debugLog command >> fn command
        in runAgentStateTAsync state' (sendChan replyCh) ma

-- | handle RPC calls that do not mutate state out of band in stateful process
-- in an EitherT monad - will log (Left reason) on failure
agentRpcAsyncHandlerET :: (NFSerializable a, NFSerializable b, NFSerializable e
                          ,Show a, Show e)
                       => (a -> (EitherT e (StateT s Agent)) b)
                       -> Dispatcher (AgentState s)
agentRpcAsyncHandlerET fn =
    handleRpcChan $ \ state' replyCh command ->
        let ma = debugLog command >> handleET fn command
        in runAgentStateTAsync state' (sendChan replyCh) ma

-- | handle calls that may mutate the State environment in
-- an EitherT monad; on Left the command and error will be logged
agentRpcHandlerET :: (NFSerializable a, NFSerializable b, NFSerializable e
                     ,Show a, Show e)
                   => (a -> (EitherT e (StateT s Agent)) b)
                   -> Dispatcher (AgentState s)
agentRpcHandlerET fn =
    handleRpcChan $ \ state' replyCh command ->
        let ma = debugLog command >> handleET fn command
        in runAgentStateT state' (sendChan replyCh) ma

-- | wrapper for commands that will handle a cast and mutate state -
-- the updated StateT environment will provide the state value to
-- 'continue'
agentCastHandler :: (NFSerializable a, Show a)
                 => (a -> (StateT s Agent) ())
                 -> Dispatcher (AgentState s)
agentCastHandler fn =
    handleCast $ \ state' command ->
        let ma = debugLog command >> fn command
        in runAgentStateT state' (const $ return ()) ma

-- | wrapper for commands that will handle a cast and mutate state
-- in an EitherT monad.
-- The updated StateT environment will provide the state value to
-- 'continue'; a Left value will log the command and error message.
agentCastHandlerET :: (NFSerializable a, Show a, NFSerializable e, Show e)
                 => (a -> (EitherT e (StateT s Agent)) ())
                 -> Dispatcher (AgentState s)
agentCastHandlerET fn =
    handleCast $ \ state' command ->
        let ma = debugLog command >> handleET fn command
        in runAgentStateT state' (const $ return ()) ma

-- | wrapper for commands that will handle an info message
-- the updated StateT environment will provide the state value to
-- 'continue'
agentInfoHandler :: (NFSerializable a, Show a)
                 => (a -> (StateT s Agent) ())
                 -> DeferredDispatcher (AgentState s)
agentInfoHandler fn =
    handleInfo $ \ state' command ->
        let ma = debugLog command >> fn command
        in runAgentStateT state' (const $ return ()) ma

runAgentStateT :: (NFSerializable a)
               => AgentState s
               -> (a -> Agent ())
               -> StateT s Agent a
               -> Process (ProcessAction (AgentState s) )
runAgentStateT (AgentState ctxt ustate) respond' ma = do
    newState <- withAgent ctxt $ do
        (result, newState) <- runStateT ma ustate
        respond' result
        return newState
    continue $ AgentState ctxt newState

runAgentStateTAsync :: (NFSerializable a)
                    => AgentState s
                    -> (a -> Agent ())
                    -> StateT s Agent a
                    -> Process (ProcessAction (AgentState s) )
runAgentStateTAsync state'@(AgentState ctxt ustate) respond' ma = do
    pid <- spawnAgent ctxt $ do
        threadDelay 1000 -- so we have time to setup a link in parent
        evalStateT ma ustate >>= respond'
    -- TODO: This will prevent the client call from hanging forever
    -- on an exception/crash but it would be better for executive to monitor child process
    -- and send an exit to the sendPortProcessId
    liftProcess $ linkOnFailure pid
    continue state'

handleET :: (NFSerializable a, NFSerializable b, NFSerializable e
                     ,Show a, Show e)
         => (a -> (EitherT e (StateT s Agent)) b) -> a
         -> (StateT s Agent) (Either e b)
handleET fn command =
    runEitherT (fn command) >>= logEitherT
  where logEitherT left'@(Left reason) = do
            [qwarn| Processing for #{command} failed with reason: #{reason} |]
            return left'
        logEitherT right' = return right'

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
agentExitHandler fn (AgentState ctxt ustate) pid reason = do
    newState <- withAgent ctxt $ do
        [qdebug| Handling exit reason #{reason}|]
        execStateT (fn pid reason) ustate
    continue $ AgentState ctxt newState

debugLog :: (Show a, MonadLogger m) => a -> m ()
debugLog c = [qdebug| Processing command: #{c}|]
