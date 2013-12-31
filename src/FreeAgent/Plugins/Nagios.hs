{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Plugins.Nagios
    ( Command(..)
    , CheckTCP(..)
    , CommandResult(..)
    , NagiosResult(..)
    , NagiosConfig(..)
    , pluginDef
    ) where

import           FreeAgent.Action
import           FreeAgent.Core
import           FreeAgent.Lenses
import           FreeAgent.Prelude

import           Data.Time.Clock   (getCurrentTime)
import           System.Exit       (ExitCode (..))
import           System.Process    (readProcessWithExitCode)

import           Data.Default      (Default (..))


-- | Plugin-specific configuration
data NagiosConfig = NagiosConfig {_nagiosPluginsPath :: FilePath}
                        deriving (Show, Eq, Typeable, Generic)

instance Default NagiosConfig where
    def = NagiosConfig "/usr/lib/nagios/plugins/"

makeFields ''NagiosConfig

-- | Supported Actions
data Command = Command { _commandHost         :: Text
                       , _commandPort         :: Maybe Int
                       , _commandShellCommand :: Text
                       } deriving (Show, Eq, Typeable, Generic)
makeFields ''Command
deriveSerializers ''Command

instance Stashable Command where
    key cmd = fromT $ cmd^.host

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
  = NagiosResult ResultSummary CommandResult
  deriving (Show, Eq, Typeable, Generic)
deriveSerializers ''NagiosResult

pluginDef :: NagiosConfig -> PluginDef
pluginDef conf = definePlugin "Nagios" conf (return []) (return []) $
 do register (actionType :: Command)
    register (actionType :: CheckTCP)

extractConfig' :: (ContextReader m) => m NagiosConfig
extractConfig' = extractConfig $ pluginDef def ^.name

instance Stashable NagiosResult where
    key (NagiosResult (ResultSummary time _ _) _) = utcToBytes time

instance Resulting NagiosResult where
    summary (NagiosResult s _) = s

instance Runnable Command NagiosResult where
    exec cmd =
        catchAny (
             do cmdPath <- commandPath
                (rc, result, _) <- liftIO $ readProcessWithExitCode cmdPath makeArgs []
                case rc of
                    ExitSuccess   -> completeAs OK result
                    ExitFailure 1 -> completeAs Warning result
                    ExitFailure 2 -> completeAs Critical result
                    ExitFailure i -> return $ Left $ tshow i ++ ": " ++ toT result )
             (\ exception -> do
                putStrLn $ "Command exec threw exception: " ++ tshow exception
                return $ Left $ tshow exception )
      where
        makeArgs = ["-H", fromT $ cmd^.host, "-p", portS $ cmd^.port]
        portS (Just p) = showStr p
        portS Nothing = ""
        completeAs :: (MonadProcess m, ContextReader m) => CommandResult -> String -> m (Either Text NagiosResult)
        completeAs cmdres result
          = do time <- liftIO getCurrentTime
               let summ = ResultSummary time (toT result) (Action cmd)
               return $ Right $ NagiosResult summ cmdres
        commandPath = do
            nagconf <- extractConfig'
            let cmdPath = (nagconf^.pluginsPath) </> fromT (cmd^.shellCommand)
            return $ fpToString cmdPath

instance Stashable CheckTCP where
    key c = fromT $ c^.host ++ ":" ++ tshow (c^.port)

instance Runnable CheckTCP NagiosResult where
    exec cmd = exec $ Command (cmd^.host) (Just $ cmd^.port) "check_tcp"
