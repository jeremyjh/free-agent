{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Cli.Main
    ( faMain
    , daemonMain
    , clientMain
    , def
    ) where

import FreeAgent.AgentPrelude
import FreeAgent.Cli.CmdArgs  (Args (..), configArgs)
import FreeAgent.Cli.Import   (importActions)
import FreeAgent.Core         (AgentConfig, PluginSet, runAgent, connectTarget)
import FreeAgent.Core.Lenses
import FreeAgent.Server       (runAgentServers)

faMain :: AgentConfig -> PluginSet -> IO ()
faMain config plugins =
 do (config', args) <- configArgs config
    case args of
        Daemon{} -> daemonMain config' plugins
        _        -> clientMain config' plugins args

daemonMain :: AgentConfig -> PluginSet -> IO ()
daemonMain config plugins =
    runAgentServers config plugins $
     do putStrLn ("Agent server listening on: "
           ++ convert (config ^. nodeHost) ++ ": "
           ++ convert (config ^. nodePort))
        putStrLn "Press enter key to stop."
        input <- asText <$> getLine
        return ()

clientMain :: AgentConfig -> PluginSet -> Args -> IO ()
clientMain config plugins args =
    let config' = config & nodeHost .~ "localhost"
                         & nodePort .~ "3547"
    in runAgent config' plugins $ connectTarget node $
         case args of
            Import _ _ path' ->
             do eimported <- importActions (convert path')
                case eimported of
                    Right () -> putStrLn "Import successfull!"
                    Left reason -> putStrLn ("Import failed: " ++ reason)
            _        -> error "mode not handled in clientMain"
  where
    node = cHost args ++ ":" ++ cPort args

