{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Process.ManagedAgent
    ( module FreeAgent.Process.ManagedAgent
    , module Managed
    , module Supervisor
    , Addressable
    , Resolvable(..)
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
import           Control.Error                                       (EitherT, runEitherT)

import Control.Distributed.Process.Platform
       (whereisOrStart, linkOnFailure, Addressable, Resolvable(..))
import           Control.Distributed.Process.Platform.Supervisor    as Supervisor
import           Control.Distributed.Process.Platform.Time          (Delay(..), milliSeconds)

data AgentState a = AgentState AgentContext a

instance Resolvable AgentServer where
    resolve (AgentServer sname _ _) = whereis sname

instance Addressable AgentServer

-- | starts the defined server process if it is not already running
startServer :: (MonadAgent m) => AgentServer -> m ProcessId
startServer (AgentServer sname sinit _) = do
    ctxt <- askContext
    liftProcess $ whereisOrStart sname (sinit ctxt)

initState :: AgentContext -> a -> Process (InitResult (AgentState a))
initState ctxt state' = return $ InitOk (AgentState ctxt state') Infinity

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
