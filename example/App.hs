{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import FreeAgent

-- import all your plugins here
import FreeAgent.Plugins.Nagios as Nagios

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentConfig
appConfig = def {
    --register all your plugin actions here
      _configPlugins = registerAll $ do
          Nagios.registerActions
          Nagios.registerActions -- different plugin goes here!
    , _configPluginConfigs = Nagios.registerConfig
    , _configDbPath = "/tmp/leveltest"
}

main :: IO ()
main = freeAgentMain (appConfig)
