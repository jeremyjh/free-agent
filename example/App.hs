{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import FreeAgent

-- import all your plugins here
import FreeAgent.Plugins.Nagios as Nagios

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentContext
appConfig = (
    registerPlugins $ do
        addPlugin $ Nagios.pluginDef def {
            -- override default plugin-specific config
            _nagiosPluginsPath = "/usr/lib/nagios/plugins/"
        }
    -- add more plugins here!
    ) { -- override Agent config values here!
        _configDbPath = "/tmp/leveltest"
    }

main :: IO ()
main = freeAgentMain (appConfig)
