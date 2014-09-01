{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FreeAgent.Cli.CmdArgs (configArgs, Args(..)) where

import FreeAgent.AgentPrelude
import FreeAgent (AgentConfig(..))
import FreeAgent.Core.Internal.Lenses hiding (summary)

import Data.List.Split (splitOn)
import Control.Lens ((&), (.~))
import System.Console.CmdArgs.Implicit


data Args = Args {
      argsServer :: String
    , argsDbPath :: String
    , argsDaemon :: Bool
    } deriving (Show, Data, Typeable)

-- | Take an AgentConfig with initial values to use for default values when
-- building args or showing help. Returns an AgentConfig which has been
-- modified with any command args passed to the progam.
configArgs :: AgentConfig -> IO (AgentConfig, Args)
configArgs dconf = cmdArgs args >>= \ args' ->
        return   ( dconf & nodeHost .~ fst (splitHost (argsServer args'))
                         & nodePort .~ snd (splitHost (argsServer args'))
                         & dbPath .~ convert (argsDbPath args')
                 , args')
  where
    args = Args { argsServer = server
                    &= help ("server:port to start or connect to - default " ++ server)
                , argsDbPath = convert db
                    &= help ("database filepath (for daemon) - default " ++ convert db)
                , argsDaemon = True
                    &= help "start a daemon if it is not presently running - default True"
                }
             &= summary "Free Agent command-line library."
    server = (dconf^.nodeHost) ++ ":" ++ (dconf^.nodePort)
    db = configDbPath dconf
    splitHost server' = let (host : port : []) = splitOn ":" server' in (host,port)
