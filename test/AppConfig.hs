{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module AppConfig (appConfig) where

import FreeAgent
-- import all your plugins here
import FreeAgent.Plugins.Nagios as Nagios

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentContext
appConfig = (
    registerPlugins $ do
        addPlugin Nagios.pluginDef
    -- add more plugins here!
    ) { -- override config values here!
        _configDbPath = "/tmp/leveltest"
    }
