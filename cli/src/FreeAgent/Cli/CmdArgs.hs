{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}

module FreeAgent.Cli.CmdArgs
    ( configArgs
    , Args(..)
   ) where

import FreeAgent.AgentPrelude
import FreeAgent.Core                  (AgentConfig (..))
import FreeAgent.Core.Lenses           hiding (name)


import System.Console.CmdArgs.Implicit (Data, cmdArgs, explicit, help, modes, name,
                                        program, typ, (&=))

data Args =  Daemon    { argsHost      :: String
                       , argsPort      :: String
                       , argsDbPath    :: String
                       , argsScheduler :: Bool
          }| Import    { argsHost      :: String
                       , argsPort      :: String
                       , argsPath      :: String
          }| Export    { argsHost      :: String
                       , argsPort      :: String
                       , argsPath      :: String
          }| Scheduler { argsHost      :: String
                       , argsPort      :: String
                       , argsStart     :: Bool
                       , argsStop      :: Bool
                       }
    deriving (Show, Data, Typeable)


-- | Take an AgentConfig with initial values to use for default values when
-- building args or showing help. Returns an AgentConfig which has been
-- modified with any command args passed to the program.
configArgs :: AgentConfig -> IO (AgentConfig, Args)
configArgs dconf = cmdArgs configModes >>= \ args' ->
        let commons = dconf & nodeHost .~ argsHost args'
                            & nodePort .~ argsPort args'
        in return (case args' of
                      Daemon _ _ path' _ -> commons & dbPath .~ convert path'
                                                    & initScheduler .~ not (argsScheduler args')
                      _                  -> commons
                  , args')
  where
    configModes =
        modes [ Import { argsHost = host
                    &= explicit &= name "host" &= typ "ADDRESS"
                    &= help ("hostname or IP to listen on or connect to - default " ++ host)
                , argsPort = port &= typ "NUM"
                    &= explicit &= name "port"
                    &= help ("service name or port to listen on or connect to - default " ++ port)
                , argsPath = "./import/"
                    &= explicit &= name "path" &= typ "DIR"
                    &= help "Path from which to import YAML files - default ./import/"
              },Export { argsHost = host
                    &= explicit &= name "host" &= typ "ADDRESS"
                    &= help ("hostname or IP to listen on or connect to - default " ++ host)
                , argsPort = port &= typ "NUM"
                    &= explicit &= name "port"
                    &= help ("service name or port to listen on or connect to - default " ++ port)
                , argsPath = "./export/" &= typ "DIR"
                    &= explicit &= name "path"
                    &= help "Path to which to export YAML files - default ./export/"
              },Daemon { argsHost = host
                    &= explicit &= name "host" &= typ "ADDRESS"
                    &= help ("hostname or IP to listen on or connect to - default " ++ host)
                , argsPort = port &= typ "NUM"
                    &= explicit &= name "port"
                    &= help ("service name or port to listen on or connect to - default " ++ port)
                , argsDbPath = path &= typ "DIR"
                    &= explicit &= name "db-path"
                    &= help ("database filepath (for daemon) - default " ++ path)
                , argsScheduler = False &= typ "BOOL"
                   &= name "no-scheduler"
                   &= help "do not start the scheduler automatically when the daemon is started - default false"
              }, Scheduler { argsHost = host
                               &= explicit &= name "host" &= typ "ADDRESS"
                               &= help ("hostname or IP to listen on or connect to - default " ++ host)
                           , argsPort = port &= typ "NUM"
                               &= explicit &= name "port"
                               &= help ("service name or port to listen on or connect to - default " ++ port)
                           , argsStart = False &= name "start" &= help "start the scheduler of a running daemon"
                           , argsStop = False &= name "stop" &= help "stop the scheduler of a running daemon"
              }] &= program "fabin"

    host = dconf ^. nodeHost
    port = dconf ^. nodePort
    path = convert (dconf ^. dbPath)


