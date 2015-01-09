{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}

module FreeAgent.Server
    ( runAgentServers
    , coreServers
    , execServer
    , peerServer
    , scheduleServer
    , defaultHistoryServer
    )
where

import FreeAgent.AgentPrelude
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Process
import FreeAgent.Core (runAgent)
import FreeAgent.Core.Server.Executive (execServer)
import FreeAgent.Core.Server.Executive.History (defaultHistoryServer)
import FreeAgent.Core.Server.Schedule (scheduleServer)
import FreeAgent.Core.Server.Peer (peerServer)
import FreeAgent.Core.Protocol.Peer (registerServer)

import Control.Distributed.Process.Platform.Supervisor

-- | Same as 'runAgent' but first starts core and plugin server processes
-- and then shuts them down cleanly.
runAgentServers :: AgentConfig -> PluginSet -> Agent () -> IO ()
runAgentServers config' plugins' ma =
    let cservers = filter (inPlugins pluginServers) coreServers
    in runAgent config' plugins' $
     do pid <- startSuper (join [pluginServers, cservers])
        ma >> liftP (shutdownAndWait pid)
  where
    inPlugins pservers cserver =
        all (\s -> s ^. name /= cserver ^. name) pservers

    pluginServers = let plugs = plugins' ^. plugins in
        concatMap (view servers) plugs

    startSuper servers' = do
        context' <- (targetServer .~ Local) <$> askContext
        spid <- liftP $ do
            cspecs <- sequence $ fmap (childFrom context') servers'
            start restartOne ParallelShutdown cspecs
        forM_ servers' $ \server' -> do
            pid <- waitRegistration $ server' ^. name
            Right () <- registerServer server' pid
            return ()
        return spid
      where childFrom context' (AgentServer _ child) = child context'

-- | Servers that are required for most use cases
coreServers :: [AgentServer]
coreServers = [peerServer, execServer, defaultHistoryServer, scheduleServer]
