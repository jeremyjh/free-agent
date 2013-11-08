{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module AppConfig (appConfig) where

import Dash
import Dash.Action

-- import all your plugins here
import Dash.Plugins.Nagios as Nagios

appConfig :: AgentConfig
appConfig = def { _configPlugins = registerAll $ do
                      Nagios.registerActions
                      Nagios.registerActions -- different plugin goes here!
                , _configPluginConfigs = Nagios.registerConfig
                , _configDbPath = "/tmp/leveltest"
                }
