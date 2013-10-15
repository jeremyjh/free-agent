{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module AppConfig (appConfig) where

import Dash.Prelude
import Dash.Types

-- import all your plugins here
import Dash.Plugins.Nagios as Nagios

appConfig :: AgentConfig
appConfig = def { _configPlugins = registerAll
             -- , _nextSettingHere = value
                }

registerAll :: PluginMap
registerAll = fromList $
    Nagios.registerUnWrappers
    -- ++ nextUnWrapper
