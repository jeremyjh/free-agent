{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import FreeAgent.AgentPrelude
import FreeAgent.Core
import FreeAgent.Core.Internal.Types (LogLevel(..))
import FreeAgent.Cli.CmdArgs (Args(..))
import FreeAgent.Cli.Main
import FreeAgent.Core.Lenses
import FreeAgent.Server.Schedule

import FreeAgent.Plugins.Nagios as Nagios

import Data.Aeson
import FreeAgent.Server.Executive
import FreeAgent.Server.ManagedAgent (callServ)
import Data.HashMap.Strict ((!))

-- | Specify the plugins that should be loaded for this application.
appPlugins :: PluginSet
appPlugins =
    pluginSet (
        addPlugin $ Nagios.pluginDef def {
            nagiosPluginsPath = "/usr/lib/nagios/plugins/"
        }
    -- add more plugins here!
    )

appConfig :: AgentConfig
appConfig = def & dbPath .~ "/tmp/examples-famon"
                & nodePort .~ "8979"
               {-& minLogLevel .~ LevelDebug-}

main :: IO ()
main = faMain appConfig appPlugins
