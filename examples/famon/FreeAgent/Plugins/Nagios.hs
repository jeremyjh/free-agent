{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}


module FreeAgent.Plugins.Nagios
    ( Command(..)
    , CheckTCP(..)
    , CommandResult(..)
    , NagiosResult(..)
    , NagiosConfig(..)
    , pluginDef
    ) where

import           FreeAgent.Core.Action
import           FreeAgent.Core.Action.ShellCommand
import           FreeAgent.Core
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.AgentPrelude


import           Data.Default      (Default (..))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Concurrent (forkIO)

-- | Plugin-specific configuration
data NagiosConfig = NagiosConfig {nagiosPluginsPath :: FilePath}
                        deriving (Show, Eq, Typeable, Generic)

instance Default NagiosConfig where
    def = NagiosConfig "/usr/lib/nagios/plugins/"

makeFields ''NagiosConfig

-- | Supported Actions
data Command = Command { _commandHost         :: Text
                       , _commandPort         :: Maybe Int
                       , _commandShellCom     :: Text
                       } deriving (Show, Eq, Typeable, Generic)
makeFields ''Command
deriveSerializers ''Command

instance Stashable Command where
    key cmd = cmd ^. host

data CommandResult = OK | Warning | Critical | Unknown
    deriving (Show, Eq, Typeable, Generic)
deriveSerializers ''CommandResult

-- | Provides the PluginDef for the Nagios plugin. Provide this to
-- 'addPlugin' in the 'registerPlugins' block in your app config/main.
-- Provide a NagiosConfig record - use 'def' for default values
--
-- > addPlugin $ Nagios.pluginDef def { _nagiosPluginPath = ... }
data CheckTCP = CheckTCP { _checktcpHost :: Text
                         , _checktcpPort :: Int
                         } deriving (Show, Eq, Typeable, Generic)
makeFields ''CheckTCP
deriveSerializers ''CheckTCP

data NagiosResult
  = NagiosResult {_nagresResultSummary :: ResultSummary
                 ,_nagresResult :: CommandResult
                 }
  deriving (Show, Eq, Typeable, Generic)
makeFields ''NagiosResult
deriveSerializers ''NagiosResult

pluginDef :: NagiosConfig -> PluginDef
pluginDef conf = definePlugin "Nagios" conf (return []) [] $
 do registerAction (actionType :: Proxy Command)
    registerAction (actionType :: Proxy CheckTCP)

extractConfig' :: (ContextReader m) => m NagiosConfig
extractConfig' = extractConfig $ pluginDef def ^. name

instance Stashable NagiosResult where
    key = key . summary

instance Resulting NagiosResult where
    summary (NagiosResult s _) = s

instance Extractable Command
instance Extractable NagiosResult

instance Runnable Command NagiosResult where
    exec cmd =
     do cmdPath <- commandPath
        let shell = (defaultShellCommand (key cmd)) {
                      shellCommand      = cmdPath
                    , shellArgs         = makeArgs
                    , shellSuccessCodes = [0,1,2]
                    }
        runEitherT $
         do result' <- tryExecET shell
            let nagresult = NagiosResult (summary result')
            return $ case shellExitCode result' of
                0 -> nagresult OK
                1 -> nagresult Warning
                2 -> nagresult Critical
                _ -> error "ShellCommand should have failed ExitCode match."
      where
        makeArgs = ["-H", cmd ^. host, "-p", portS $ cmd ^. port]
        portS (Just p) = tshow p
        portS Nothing = ""
        commandPath = do
            nagconf <- extractConfig'
            return $ nagiosPluginsPath nagconf </> convert (cmd ^. shellCom)

instance Stashable CheckTCP where
    key c = c ^. host ++ ":" ++ tshow (c ^. port)

instance Extractable CheckTCP

instance Runnable CheckTCP NagiosResult where
    exec cmd =
        {-do (ExitSuccess, _, _) <- liftIO $ readProcessWithExitCode "/bin/sleep" ["5"] ""-}
           {-return (Right undefined)-}
        runEitherT $
        (resultSummary.resultOf .~ Action cmd) <$>
            tryExecET (Command (cmd ^. host) (Just $ cmd ^. port) "check_tcp")
