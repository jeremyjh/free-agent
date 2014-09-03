{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module FreeAgent.Cli.Main
    ( faMain
    , daemonMain
    , clientMain
    , def
    ) where

import FreeAgent.AgentPrelude
import FreeAgent.Cli.CmdArgs
import FreeAgent.Core
import FreeAgent.Core.Lenses
import FreeAgent.Server

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
    in runAgent config' plugins $
         case args of
            Import{} -> putStrLn "doing import"
            _        -> error "mode not handled in clientMain"

