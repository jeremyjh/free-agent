{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module AppConfig (appConfig) where

import FreeAgent
import FreeAgent.Lenses
-- import all your plugins here
import FreeAgent.Plugins.Nagios as Nagios

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentContext
appConfig = (
    registerPlugins def $ do
        addPlugin $ Nagios.pluginDef def {
            -- override default plugin-specific config
            _nagiosPluginsPath = "/usr/lib/nagios/plugins"
        }
        -- add more plugins here!
    ) & agentConfig.dbPath .~ "/tmp/leveltest" -- override Agent config values here!
      {-& agentConfig.minLogLevel .~ LevelDebug-}
