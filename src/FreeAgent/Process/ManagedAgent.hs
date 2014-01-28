{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module FreeAgent.Process.ManagedAgent
    ( module FreeAgent.Process.ManagedAgent
    , module Managed
    )

where

import           FreeAgent.Core                                      (spawnAgent, withAgent)
import           FreeAgent.Lenses
import           FreeAgent.Process

import           Control.Monad.State                                 (StateT, evalStateT, execStateT)

import           Control.Concurrent.Lifted                           (threadDelay)
import           Control.Distributed.Process.Platform.ManagedProcess as Managed
                                                                    hiding (cast, call, syncCallChan)

import           Control.Distributed.Process.Platform               (whereisOrStart,linkOnFailure)
import           Control.Distributed.Process.Platform.Time          (Delay(..))

data AgentState a = AgentState AgentContext a


-- | starts the defined server process if it is not already running
-- and sends the supplied argument with (NF) syncCallChan
callServer :: (MonadAgent m, NFSerializable a, NFSerializable b) => AgentServer -> a -> m b
callServer server cmd = do
    ctxt <- askContext
    pid <- startServer server
    syncCallChan pid cmd

-- | starts the defined server process if it is not already running
startServer :: (MonadAgent m) => AgentServer -> m ProcessId
startServer (AgentServer sname sinit _) = do
    ctxt <- askContext
    pid <- liftProcess $ whereisOrStart sname (sinit ctxt)
    return pid

initState :: AgentContext -> a -> Process (InitResult (AgentState a))
initState ctxt state' = return $ InitOk (AgentState ctxt state') Infinity

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
        -- | TODO: This will prevent the client call from hanging forever
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
    withStateAgent (AgentState ctxt state') ma = do
        withAgent ctxt $ do
            state'' <- execStateT ma state'
            return $ AgentState ctxt state''
