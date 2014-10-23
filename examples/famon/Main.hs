{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import FreeAgent.AgentPrelude
import FreeAgent.Cli.CmdArgs                      (Args (..))
import FreeAgent.Cli.Main                         (clientMain, daemonMain, exitDaemon,
                                                   faMain)
import FreeAgent.Core
import FreeAgent.Core.Internal.Types              (LogLevel (..))
import FreeAgent.Core.Lenses
import FreeAgent.Process                          (liftProcess)

import FreeAgent.Plugins.Nagios                   as Nagios

import Control.Distributed.Process.Platform.Time  (seconds, TimeUnit(..))
import Control.Distributed.Process.Platform.Timer (runAfter, sleepFor)

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
                & minLogLevel .~ LevelInfo

main :: IO ()
main =
 do args <- getArgs
    case args of
        "bench" : _ -> daemonMain appConfig appPlugins $ liftProcess $
         do sleepFor 500 Millis -- wait for Peer to catch up
            void $ runAfter (seconds 30) exitDaemon
        _ -> faMain appConfig appPlugins
