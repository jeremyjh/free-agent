{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}

module FreeAgent.Cli.CmdArgs
    ( configArgs
    , Args(..)
   ) where

import FreeAgent.AgentPrelude
import FreeAgent.Core                  (AgentConfig (..))
import FreeAgent.Core.Lenses           hiding (name)

import Data.List.Split                 (splitOn)

import System.Console.CmdArgs.Implicit (Data, cmdArgs, explicit, help, modes,
                                        name, program, summary, typ, (&=))

data Args =  Daemon { cHost   :: String
                    , cPort   :: String
                    , cDbPath :: String
          }| Import { cHost      :: String
                    , cPort      :: String
                    , importPath :: String
                    }
    deriving (Show, Data, Typeable)


-- | Take an AgentConfig with initial values to use for default values when
-- building args or showing help. Returns an AgentConfig which has been
-- modified with any command args passed to the program.
configArgs :: AgentConfig -> IO (AgentConfig, Args)
configArgs dconf = cmdArgs configModes >>= \ args' ->
        let commons = dconf & nodeHost .~ cHost args'
                            & nodePort .~ cPort args'
        in return (case args' of
                      Daemon _ _ path' -> commons & dbPath .~ convert path'
                      _                -> commons
                  , args')
  where
    configModes =
        modes [ Import { cHost = host
                    &= explicit &= name "host" &= typ "ADDRESS"
                    &= help ("hostname or IP to listen on or connect to - default " ++ host)
                , cPort = port &= typ "NUM"
                    &= explicit &= name "port"
                    &= help ("service name or port to listen on or connect to - default " ++ port)
                , importPath = "./import/"
                    &= explicit &= name "import-path" &= typ "DIR"
                    &= help "Path from which to import YAML files."
              },Daemon { cHost = host
                    &= explicit &= name "host" &= typ "ADDRESS"
                    &= help ("hostname or IP to listen on or connect to - default " ++ host)
                , cPort = port &= typ "NUM"
                    &= explicit &= name "port"
                    &= help ("service name or port to listen on or connect to - default " ++ port)
                , cDbPath = path &= typ "DIR"
                    &= explicit &= name "db-path"
                    &= help ("database filepath (for daemon) - default " ++ path)
              }] &= program "fabin"

    host = dconf ^. nodeHost
    port = dconf ^. nodePort
    path = convert (dconf ^. dbPath)
