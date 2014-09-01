{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FreeAgent.Cli.CmdArgs (configArgs, Args(..)) where

import FreeAgent.AgentPrelude
import FreeAgent (AgentConfig(..))
import FreeAgent.Core.Lenses hiding (name)

import Data.List.Split (splitOn)
import System.Console.CmdArgs.Implicit


data Args = Args {
      argsHost :: String
    , argsPort :: String
    , argsDbPath :: String
    , argsDaemon :: Bool
    } deriving (Show, Data, Typeable)

-- | Take an AgentConfig with initial values to use for default values when
-- building args or showing help. Returns an AgentConfig which has been
-- modified with any command args passed to the progam.
configArgs :: AgentConfig -> IO (AgentConfig, Args)
configArgs dconf = cmdArgs args >>= \ args' ->
        return   ( dconf & nodeHost .~ argsHost args'
                         & nodePort .~ argsPort args'
                         & dbPath .~ convert (argsDbPath args')
                 , args')
  where
    args = Args { argsHost = host
                    &= explicit &= name "host" &= typ "ADDRESS"
                    &= help ("hostname or IP to listen on or connect to - default " ++ host)
                , argsPort = port &= typ "NUM"
                    &= explicit &= name "port"
                    &= help ("service name or port to listen on or connect to - default " ++ port)
                , argsDbPath = path &= typ "DIR"
                    &= explicit &= name "db-path"
                    &= help ("database filepath (for daemon) - default " ++ path)
                , argsDaemon = False
                    &= explicit &= name "daemon"
                    &= help "start a daemon listening on local host address and port"
                }
             &= summary "Free Agent command-line library."
    host = dconf ^. nodeHost
    port = dconf ^. nodePort
    path = convert (dconf ^. dbPath)
