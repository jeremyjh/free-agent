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
import FreeAgent.Server.Schedule (scheduleServer)
import FreeAgent.Server.Peer (peerServer, registerServer)

import Control.Distributed.Process.Platform.Supervisor

-- | Same as 'runAgent' but first starts core and plugin server processes
runAgentServers :: AgentConfig -> PluginSet -> Agent () -> IO ()
runAgentServers config' plugins' ma =
    let pservers = pluginServers plugins'
        cservers = filter (inPlugins pservers) coreServers
    in runAgent config' plugins' $ startSuper (join [pservers, cservers]) >> ma
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
    forM_ servers' $ \server' -> do
        pid <- liftProcess $ do
            child' <- childFrom context' server'
            void $ start restartOne ParallelShutdown [child']
            waitRegistration $ server'^.name
        Right () <- registerServer server' pid
        return ()
  where childFrom context' (AgentServer _ child) = child context'

-- | Servers that are required for most use cases
coreServers :: [AgentServer]
coreServers = [peerServer, execServer, defaultHistoryServer, scheduleServer]

pluginServers :: PluginSet -> [AgentServer]
pluginServers plugins' = let plugs = plugins'^.plugins in
    concatMap (view servers) plugs
