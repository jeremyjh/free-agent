{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import Dash

-- import all your plugins here
import Dash.Plugins.Nagios as Nagios

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentConfig
appConfig = def { _configPlugins = registerAll
             -- , _nextSettingHere = value
                }

-- Register each plugin here by appending it's registerUnWrappers
registerAll :: PluginMap
registerAll = fromList $
    Nagios.registerUnWrappers
    -- ++ Another.registerUnWrappers


main :: IO ()
main = dashMain (appConfig)
