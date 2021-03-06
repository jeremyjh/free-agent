{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import           FreeAgent.AgentPrelude
import FreeAgent.Cli.Main                         (daemonMain, exitDaemon,
                                                   faMain)
import           FreeAgent.Core
import           FreeAgent.Core.Internal.Types (LogLevel (..))
import           FreeAgent.Core.Lenses

import           FreeAgent.Plugins.Nagios as Nagios

import           Control.Distributed.Process.Extras.Time (TimeUnit (..), seconds)
import           Control.Distributed.Process.Extras.Timer (runAfter, sleepFor)

--derp

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
        "bench" : "init" : num : _ -> benchInit num
        "bench" : _                -> bench
        _                          -> faMain appConfig appPlugins

bench :: IO ()
bench =
    daemonMain appConfig appPlugins $ liftP $
     do sleepFor 500 Millis -- wait for Peer to catch up
        void $ runAfter (seconds 30) exitDaemon
        return True

benchInit :: Text -> IO ()
benchInit num =
    daemonMain appConfig {configInitScheduler = False} appPlugins $
        let inum = read num :: Int
            bkey idx = "benc_check_procs_" ++ tshow idx
            keys = map bkey [1..inum]
            checks key' (actions, events) =
                ( (toAction $ benchCheckProcs {procKey = key'}) : actions
                , Event key' (RecurInterval 1) Never zeroDate False : events )
            (actions, events) = foldr checks ([],[]) keys
        in do putStrLn ("Creating " ++ num ++ " Actions and Events.")
              liftP $ sleepFor 500 Millis
              Right () <- callServ (StoreActions actions)
              Right () <- callServ (ScheduleAddEvents events)
              Right () <- callServ (ScheduleEnableEvents keys)
              return False

benchCheckProcs :: CheckProcs
benchCheckProcs = CheckProcs "bench_check_procs" "famon" 1 1 2 1024
