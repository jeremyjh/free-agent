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
import Control.Concurrent.Lifted (threadDelay)


-- | Same as 'runAgent' but first starts core server processes
runAgentServers :: AgentContext -> Agent () -> IO ()
runAgentServers ctxt ma = runAgent ctxt $ startSuper coreServers >> ma

-- | Start a supervisor as RestartOne with a list of AgentServer definitions
startSuper ::  [AgentServer] -> Agent ()
startSuper servers = do
    ctxt <- askContext
    liftProcess $ do
        cspecs <- sequence $ fmap (childFrom ctxt) servers
        void $ start restartOne cspecs
    threadDelay 10000
  where childFrom ctxt (AgentServer _ _ child) = child ctxt

-- | Servers that are required for most use cases
coreServers :: [AgentServer]
coreServers = [peerServer, execServer]
