{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Cli.Main
    ( faMain
    , daemonMain
    , clientMain
    , exitDaemon
    , def
    ) where

import FreeAgent.AgentPrelude
import FreeAgent.Cli.CmdArgs  (Args (..), configArgs)
import FreeAgent.Cli.Import
import FreeAgent.Cli.Export
import FreeAgent.Core         (AgentConfig, PluginSet, withRemoteNode, runAgent, Agent)
import FreeAgent.Core.Lenses
import FreeAgent.Process
import FreeAgent.Server       (runAgentServers)
import FreeAgent.Server.ManagedAgent (castServ)
import FreeAgent.Server.Schedule (ScheduleControl(..))

faMain :: AgentConfig -> PluginSet -> IO ()
faMain config plugins =
 do (config', args) <- configArgs config
    let execClient = clientMain config' plugins
    case args of
        Daemon{} -> daemonMain config' plugins (return True)
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
        Scheduler _ _ start stop
            | start -> execClient $
                 do estarted <- castServ ScheduleStart
                    case estarted of
                        Right () -> putStrLn "Sent start command to daemon."
                        Left reason -> putStrLn ("Could not send start command: " ++ tshow reason)
            | stop -> execClient $
                  do estopped <- castServ ScheduleStop
                     case estopped of
                         Right () -> putStrLn "Sent stop command to daemon."
                         Left reason -> putStrLn ("Could not send stop command: " ++ tshow reason)
            | otherwise -> return ()

daemonMain :: AgentConfig -> PluginSet -> Agent Bool -> IO ()
daemonMain config plugins ma =
    runAgentServers config plugins $
     do needWait <- ma
        when needWait waitExit
  where waitExit =
         do pid <- getSelfPid; register "daemon:exit" pid
            void $ spawnLocal (promptExit pid)
            "exit" <- expect :: Agent Text
            return ()
        promptExit pid =
         do putStrLn ("Agent server listening on: "
                 ++ convert (config ^. nodeHost) ++ ": "
                 ++ convert (config ^. nodePort))
            putStrLn "Press enter key to stop."
            void $ asText <$> getLine
            send pid $ asText "exit"

exitDaemon :: MonadProcess process => process ()
exitDaemon =
 do Just pid <- whereis "daemon:exit"
    putStrLn "Bench done; sending exit."
    send pid $ asText "exit"

clientMain :: AgentConfig -> PluginSet -> Agent () -> IO ()
clientMain config plugins ma =
    let node = (config ^. nodeHost) ++ ":" ++ (config ^. nodePort)
        config' = config & nodeHost .~ "localhost"
                         & nodePort .~ "3547" --TODO: needs to automatically find a free port
    in runAgent config' plugins $ withRemoteNode node ma
