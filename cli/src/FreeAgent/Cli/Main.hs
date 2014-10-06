{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Cli.Main
    ( faMain
    , daemonMain
    , clientMain
    , def
    ) where

import FreeAgent.AgentPrelude
import FreeAgent.Cli.CmdArgs  (Args (..), configArgs)
import FreeAgent.Cli.Import
import FreeAgent.Cli.Export
import FreeAgent.Core         (AgentConfig, PluginSet, withRemoteNode, runAgent, Agent)
import FreeAgent.Core.Lenses
import FreeAgent.Server       (runAgentServers)
import FreeAgent.Server.ManagedAgent (castServ)
import FreeAgent.Server.Schedule (ScheduleControl(..))

faMain :: AgentConfig -> PluginSet -> IO ()
faMain config plugins =
 do (config', args) <- configArgs config
    let execClient = clientMain config' plugins
    case args of
        Daemon{} -> daemonMain config' plugins
        Import _ _ path' ->
            execClient $
             do eimported <- importActions (convert path')
                case eimported of
                    Right () -> putStrLn "Import successfull!"
                    Left reason -> putStrLn ("Import failed: " ++ reason)
        Export _ _ path' ->
            execClient $
             do eexported <- exportActions (convert path')
                case eexported of
                    Right () -> putStrLn "Export successfull!"
                    Left reason -> putStrLn ("Export failed: " ++ reason)
        Scheduler _ _ start stop ->
            execClient $
                if start then
                 do estarted <- castServ ScheduleStart
                    case estarted of
                        Right () -> putStrLn "Sent start command to daemon."
                        Left reason -> putStrLn ("Could not send start command: " ++ tshow reason)
                else when stop $
                      do estopped <- castServ ScheduleStop
                         case estopped of
                             Right () -> putStrLn "Sent stop command to daemon."
                             Left reason -> putStrLn ("Could not send stop command: " ++ tshow reason)

daemonMain :: AgentConfig -> PluginSet -> IO ()
daemonMain config plugins =
    runAgentServers config plugins $
      do putStrLn ("Agent server listening on: "
                 ++ convert (config ^. nodeHost) ++ ": "
                 ++ convert (config ^. nodePort))
         putStrLn "Press enter key to stop."
         void $ asText <$> getLine

clientMain :: AgentConfig -> PluginSet -> Agent () -> IO ()
clientMain config plugins ma =
    let node = (config ^. nodeHost) ++ ":" ++ (config ^. nodePort)
        config' = config & nodeHost .~ "localhost"
                         & nodePort .~ "3547" --TODO: needs to automatically find a free port
    in runAgent config' plugins $ withRemoteNode node ma
