{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module FreeAgent.Process.ManagedAgent
    ( module Managed
    , module Supervisor
    , AgentState(..)
    , defineServer
    , agentCastHandler
    , agentCastHandlerET
    , agentCallHandlerET
    , agentCallHandler
    , agentAsyncCallHandler
    , agentAsyncCallHandlerET
    , agentExitHandler
    , serverState
    , Addressable
    , Delay(..)
    , ChildSpec(..)
    , milliSeconds
    )

where

import           AgentPrelude
import           FreeAgent.Core                                      (spawnAgent, withAgent)
import           FreeAgent.Lenses
import           FreeAgent.Process

import           Control.Monad.State                                 (StateT, evalStateT
                                                                     , execStateT, runStateT)


import           Control.Concurrent.Lifted                           (threadDelay)
import           Control.Distributed.Process.Platform.ManagedProcess as Managed
    hiding (cast, call, syncCallChan, action, shutdown)
import           Control.Distributed.Process.Platform (linkOnFailure, Addressable)
import           Control.Distributed.Process.Platform.Supervisor    as Supervisor
import           Control.Distributed.Process.Platform.Time          (Delay(..), milliSeconds)
import           Control.Error                                       (EitherT, runEitherT)

data AgentState a = AgentState AgentContext a

serverState :: Lens' (AgentState a) a
serverState f (AgentState c a) = fmap (\a' -> AgentState c a') (f a)

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
              childKey     = ""
            , childType    = Worker
            , childRestart = Permanent
            , childStop    = TerminateTimeout (Delay $ milliSeconds 10)
            , childStart   = initChild
            , childRegName = Just $ LocalName name'
        }

-- | handle calls that may mutate the State environment
agentCallHandler :: (NFSerializable a, NFSerializable b, Show a)
                  => (a -> (StateT s Agent) b)
                  -> AgentState s -> SendPort b -> a
                  -> Process (ProcessAction (AgentState s) )
agentCallHandler fn (AgentState ctxt ustate) replyCh command = do
   newState <-  withAgent ctxt $ do
        [qdebug| Processing command: #{command}|]
        (result, newState) <- runStateT (fn command) ustate
        sendChan replyCh result
        return newState
   continue $ AgentState ctxt newState


-- | handle calls that may mutate the State environment in
-- an EitherT monad
agentCallHandlerET :: (NFSerializable a, NFSerializable b, NFSerializable e
                     ,Show a, Show e)
                  => (a -> (EitherT e (StateT s Agent)) b)
                  -> AgentState s
                  -> SendPort (Either e b)
                  -> a -> Process (ProcessAction (AgentState s) )
agentCallHandlerET fn (AgentState ctxt ustate) replyCh command = do
    newState <- withAgent ctxt $ do
        [qdebug| Processing command: #{command}|]
        let ema = runEitherT (fn command) >>= logEitherT
        (result, newState) <- runStateT ema ustate
        sendChan replyCh result
        return newState
    continue $ AgentState ctxt newState
  where
    logEitherT left'@(Left reason) = do
        [qwarn| Processing for #{command} failed with reason: #{reason} |]
        return left'
    logEitherT right' = return right'


-- | handle calls that do not mutate state out of band in stateful process
agentAsyncCallHandler :: (NFSerializable a, NFSerializable b, Show a)
                  => (a -> (StateT s Agent) b)
                  -> AgentState s -> SendPort b -> a
                  -> Process (ProcessAction (AgentState s) )
agentAsyncCallHandler fn state'@(AgentState ctxt ustate) port command = do
    pid <- spawnAgent ctxt $ do
        [qdebug| Processing command: #{command}|]
        threadDelay 1000 -- so we have time to setup a link in parent
        r <- evalStateT (fn command) ustate
        sendChan port r
    -- TODO: This will prevent the client call from hanging forever
    -- on an exception/crash but it would be better for executive to monitor child process
    -- and send an exit to the sendPortProcessId
    liftProcess $ linkOnFailure pid
    continue state'

-- | handle calls that do not mutate state out of band in stateful process
-- in an EitherT monad - will log (Left reason) on failure
agentAsyncCallHandlerET :: (NFSerializable a, NFSerializable b, NFSerializable e
                          , Show a, Show e)
                       => (a -> (EitherT e (StateT s Agent)) b)
                       -> AgentState s
                       -> SendPort (Either e b)
                       -> a -> Process (ProcessAction (AgentState s) )
agentAsyncCallHandlerET fn state'@(AgentState ctxt ustate) port command = do
    pid <- spawnAgent ctxt $ do
        [qdebug| Processing command: #{command}|]
        threadDelay 1000 -- so we have time to setup a link in parent
        let ema = runEitherT (fn command) >>= logEitherT
        r <- evalStateT ema ustate
        sendChan port r
    -- TODO: This will prevent the client call from hanging forever
    -- on an exception/crash but it would be better for executive to monitor child process
    -- and send an exit to the sendPortProcessId
    liftProcess $ linkOnFailure pid
    continue state'
  where
    logEitherT left'@(Left reason) = do
        [qwarn| Processing for #{command} failed with reason: #{reason} |]
        return left'
    logEitherT right' = return right'

-- | wrapper for commands that will handle a cast and mutate state -
-- the updated StateT environment will provide the state value to
-- 'continue'
agentCastHandler :: (NFSerializable a, Show a)
                 => (a -> (StateT s Agent) ())
                 -> AgentState s -> a
                 -> Process (ProcessAction (AgentState s) )
agentCastHandler fn (AgentState ctxt ustate) command = do
    newState <- withAgent ctxt $ do
        [qdebug| Processing command: #{command}|]
        execStateT (fn command) ustate
    continue $ AgentState ctxt newState

-- | wrapper for commands that will handle a cast and mutate state -
-- the updated StateT environment will provide the state value to
-- 'continue'
agentCastHandlerET :: (NFSerializable a, Show a, NFSerializable e, Show e)
                 => (a -> (EitherT e (StateT s Agent)) ())
                 -> AgentState s -> a
                 -> Process (ProcessAction (AgentState s) )
agentCastHandlerET fn (AgentState ctxt ustate) command = do
    newState <- withAgent ctxt $ do
        [qdebug| Processing command: #{command}|]
        let ema = runEitherT (fn command) >>= logEitherT
        execStateT ema ustate
    continue $ AgentState ctxt newState
  where
    logEitherT left'@(Left reason) = do
        [qwarn| Processing for #{command} failed with reason: #{reason} |]
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
