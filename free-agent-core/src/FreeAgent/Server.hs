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
import FreeAgent.Server.Executive.History (defaultHistoryServer)
import FreeAgent.Server.Peer (peerServer, registerServer)

import Control.Distributed.Process.Platform.Supervisor


-- | Same as 'runAgent' but first starts core and plugin server processes
runAgentServers :: AgentContext -> Agent () -> IO ()
runAgentServers context' ma =
    let pservers = pluginServers context'
        cservers = filter (inPlugins pservers) coreServers in
    runAgent context' $ startSuper (join [pservers, cservers]) >> ma
  where inPlugins pservers cserver =
            all (\s -> s^.name /= cserver^.name) pservers

-- | Start a supervisor as RestartOne with a list of AgentServer definitions
startSuper ::  [AgentServer] -> Agent ()
startSuper servers' = do
    context' <- askContext
    --TODO: I think its a bug in Supervisor that the whole group
    --is restarted even when RestartOne is selected
    {-liftProcess $ do-}
        {-cspecs <- sequence $ fmap (childFrom context') servers'-}
        {-void $ start restartOne ParallelShutdown cspecs-}
        {-pid <- waitRegistration $ (server'^.name)-}
        {-registerServer server' pid-}
    forM_ servers' $ \server' -> liftProcess $ do
        child' <- childFrom context' server'
        void $ start restartOne ParallelShutdown [child']
        when (server'^.name /= peerServer^.name) $ do
            pid <- waitRegistration $ server'^.name
            registerServer server' pid
  where childFrom context' (AgentServer _ child) = child context'

-- | Servers that are required for most use cases
coreServers :: [AgentServer]
coreServers = [peerServer, execServer, defaultHistoryServer]

pluginServers :: AgentContext -> [AgentServer]
pluginServers context' = let plugs = context'^.plugins in
    concat $ map (view servers) plugs
