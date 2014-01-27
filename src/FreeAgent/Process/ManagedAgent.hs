{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module FreeAgent.Process.ManagedAgent where

import           FreeAgent.Core                                      (spawnAgent)
import           FreeAgent.Lenses
import           FreeAgent.Process

import           Control.Monad.State                                 (StateT, evalStateT)

import           Control.Concurrent.Lifted                           (threadDelay)
import           Control.Distributed.Process.Platform.ManagedProcess as Managed
                                                                    hiding (cast, call, syncCallChan)

import           Control.Distributed.Process.Platform               (whereisOrStart,linkOnFailure)
import           Control.Distributed.Process.Platform.Time          (Delay(..))

data AgentState a = AgentState AgentContext a

data AgentServer = AgentServer String (AgentContext -> Process ())

-- | starts the defined process if it is not already running
-- and sends the supplied argument with syncCallChan
sendCommand :: (MonadAgent m, NFSerializable a, NFSerializable b) => AgentServer -> a -> m b
sendCommand (AgentServer sname sinit) cmd = do
    ctxt <- askContext
    pid <- liftProcess $ whereisOrStart sname (sinit ctxt)
    syncCallChan pid cmd

initState :: AgentContext -> a -> Process (InitResult (AgentState a))
initState ctxt state' = return $ InitOk (AgentState ctxt state') Infinity

-- | handle calls that do not mutate state out of band in stateful process
asyncAgentHandler :: (NFSerializable a, NFSerializable b)
                  => (a -> (StateT s Agent) b)
                  -> AgentState s -> SendPort b -> a
                  -> Process (ProcessAction (AgentState s) )
asyncAgentHandler f s p c = spawnStateAgent s p (f c) >> continue s
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
