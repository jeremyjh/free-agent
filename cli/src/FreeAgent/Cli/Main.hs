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
import FreeAgent.Core         (AgentConfig, PluginSet, withRemoteNode, runAgent, Agent, MonadProcess)
import FreeAgent.Core.Lenses
import FreeAgent.Process
import FreeAgent.Server       (runAgentServers)
import FreeAgent.Core.Protocol (castServ)
import FreeAgent.Core.Protocol.Schedule (ScheduleControl(..))

faMain :: AgentConfig -> PluginSet -> IO ()
faMain config plugins =
 do (config', args) <- configArgs config
    let config'' = defport config'
    let execClient = clientMain config'' plugins
    case args of
        Daemon{} -> daemonMain config'' plugins (return True)
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
  where
    defport config' = if config' ^. nodePort == ""
                      then config' & nodePort .~ "3546"
                      else config'

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
         do liftIO $
              do putStrLn ("Agent server listening on: "
                    ++ convert (config ^. nodeHost) ++ ": "
                    ++ convert (config ^. nodePort))
                 putStrLn "Press enter key to stop."
                 void getLine
            send pid  ("exit" :: Text)

exitDaemon :: MonadProcess process => process ()
exitDaemon =
 do Just pid <- whereis "daemon:exit"
    putStrLn "Bench done; sending exit."
    send pid ("exit" :: Text)

clientMain :: AgentConfig -> PluginSet -> Agent () -> IO ()
clientMain config plugins ma =
    let node = (config ^. nodeHost) ++ ":" ++ (config ^. nodePort)
        config' = config & nodeHost .~ "localhost"
                         & nodePort .~ ""
    in runAgent config' plugins $ withRemoteNode node ma
