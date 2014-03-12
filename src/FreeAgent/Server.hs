{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
module FreeAgent.Server
    ( runAgentServers
    , execServer
    , peerServer
    , coreServers)
where

import AgentPrelude
import FreeAgent.Lenses
import FreeAgent.Process
import FreeAgent.Core (runAgent)
import FreeAgent.Server.Executive (execServer)
import FreeAgent.Server.Peer (peerServer)

import Control.Distributed.Process.Platform.Supervisor


-- | Same as 'runAgent' but first starts core server processes
runAgentServers :: AgentContext -> Agent () -> IO ()
runAgentServers ctxt ma = runAgent ctxt $ startSuper coreServers >> ma

-- | Start a supervisor as RestartOne with a list of AgentServer definitions
startSuper ::  [AgentServer] -> Agent ()
startSuper servers' = do
    ctxt <- askContext
    --TODO: I think its a bug in Supervisor that the whole group
    --is restarted even when RestartOne is selected
    {-liftProcess $ do-}
        {-cspecs <- sequence $ fmap (childFrom ctxt) servers'-}
        {-void $ start restartOne ParallelShutdown cspecs-}
    forM_ servers' $ \s -> liftProcess $ do
        child' <- childFrom ctxt s
        start restartOne ParallelShutdown [child']
    forM_ servers' $ \s -> waitRegistration $ s^.name
  where childFrom ctxt (AgentServer _ _ child) = child ctxt

-- | Servers that are required for most use cases
coreServers :: [AgentServer]
coreServers = [peerServer, execServer]
