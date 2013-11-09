{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module AppConfig (appConfig) where

import Dash

-- import all your plugins here
import Dash.Plugins.Nagios as Nagios

appConfig :: AgentConfig
appConfig = def {
      _configPlugins = registerAll $ do
          -- register all your plugin actions here
          Nagios.registerActions
          Nagios.registerActions -- different plugin goes here!
    , _configPluginConfigs = Nagios.registerConfig
    , _configDbPath = "/tmp/leveltest"
}
