{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Process.ManagedAgent
    ( module FreeAgent.Process.ManagedAgent
    , module Managed
    , module Supervisor
    , Addressable(..)
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
                                                                    hiding (cast, call, syncCallChan, action)

import           Control.Distributed.Process.Platform               (whereisOrStart,linkOnFailure, Addressable(..))
import           Control.Distributed.Process.Platform.Supervisor    as Supervisor
import           Control.Distributed.Process.Platform.Time          (Delay(..), milliSeconds)

data AgentState a = AgentState AgentContext a

instance Addressable AgentServer where
    resolve (AgentServer sname _ _) = whereis sname

-- | starts the defined server process if it is not already running
startServer :: (MonadAgent m) => AgentServer -> m ProcessId
startServer (AgentServer sname sinit _) = do
    ctxt <- askContext
    liftProcess $ whereisOrStart sname (sinit ctxt)

initState :: AgentContext -> a -> Process (InitResult (AgentState a))
initState ctxt state' = return $ InitOk (AgentState ctxt state') Infinity

-- | handle calls that may mutate the State environment
agentCallHandler :: (NFSerializable a, NFSerializable b)
                  => (a -> (StateT s Agent) b)
                  -> AgentState s -> SendPort b -> a
                  -> Process (ProcessAction (AgentState s) )
agentCallHandler f s p c = agentHandler s p (f c) >>= continue
  where
    agentHandler (AgentState ctxt state') replyCh ma =
        withAgent ctxt $ do
            (result, newState) <- runStateT ma state'
            sendChan replyCh result
            return $ AgentState ctxt newState

-- | handle calls that do not mutate state out of band in stateful process
agentAsyncCallHandler :: (NFSerializable a, NFSerializable b)
                  => (a -> (StateT s Agent) b)
                  -> AgentState s -> SendPort b -> a
                  -> Process (ProcessAction (AgentState s) )
agentAsyncCallHandler f s p c = spawnStateAgent s p (f c) >> continue s
  where
    spawnStateAgent (AgentState ctxt state') port ma = do
        pid <- spawnAgent ctxt $ do
            threadDelay 1000 -- so we have time to setup a link in parent
            r <- evalStateT ma state'
            sendChan port r
        -- TODO: This will prevent the client call from hanging forever
        -- on an exception/crash but it would be better for executive to monitor child process
        -- and send an exit to the sendPortProcessId
        liftProcess $ linkOnFailure pid

-- | wrapper for commands that will handle a cast and mutate state -
-- the updated StateT environment will provide the state value to
-- 'continue'
agentCastHandler :: (NFSerializable a)
                 => (a -> (StateT s Agent) ())
                 -> AgentState s -> a
                 -> Process (ProcessAction (AgentState s) )
agentCastHandler f s c = withStateAgent s (f c) >>= continue
  where
    withStateAgent (AgentState ctxt state') ma =
        withAgent ctxt $ do
            newState <- execStateT ma state'
            return $ AgentState ctxt newState
