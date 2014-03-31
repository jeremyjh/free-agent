{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgent.TestHelper
    ( appConfig
    , appPlugins
    , testRunAgent
    , setup
    , nosetup
    ) where

import FreeAgent
import FreeAgent.Lenses
import FreeAgent.Server
-- import all your plugins here
import FreeAgent.Plugins.Nagios as Nagios
import Control.Concurrent.Lifted (threadDelay)
import System.Process (system)

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentConfig
appConfig = def & dbPath .~ "/tmp/leveltest"
                {-& minLogLevel .~ LevelDebug-}

appPlugins :: PluginSet
appPlugins =
    pluginSet $ do
        addPlugin $ Nagios.pluginDef def {
            -- override default plugin-specific config
            _nagiosPluginsPath = "/usr/lib/nagios/plugins"
        }

-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testRunAgent :: IO () -> AgentConfig -> PluginSet -> ((a -> Agent ()) -> Agent ()) -> IO a
testRunAgent doSetup config' plugins' ma = do
    doSetup
    result <- newEmptyMVar
    runAgentServers config' plugins' (ma (putMVar result))
    threadDelay 15000 -- so we dont get open port errors
    takeMVar result

setup :: IO ()
setup = void $ system ("rm -rf " ++ (convert $ appConfig^.dbPath))

nosetup :: IO ()
nosetup = return ()
