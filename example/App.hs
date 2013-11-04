{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import Dash
import Dash.Action

-- import all your plugins here
import Dash.Plugins.Nagios as Nagios

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentConfig
appConfig = def { _configPlugins = registerAll $ do
                      Nagios.registerActions
                      Nagios.registerActions -- different plugin goes here!
                }

main :: IO ()
main = dashMain (appConfig)
