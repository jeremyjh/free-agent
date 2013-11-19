{-# LANGUAGE NoImplicitPrelude, OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Plugins.Nagios
    ( Command(..)
    , CheckTCP(..)
    , NagiosResult(..)
    , NagiosConfig(..)
    , pluginDef
    ) where

import           FreeAgent.Prelude
import           FreeAgent.Lenses
import           FreeAgent.Action
import           FreeAgent.Core

import           System.Process     (readProcessWithExitCode)
import           System.Exit (ExitCode(..))

import qualified Data.Serialize                   as Cereal
import           Data.SafeCopy

import           Data.Default (Default(..))
import           Data.Binary
import           Control.Lens (makeFields)

-- | Plugin-specific configuration
data NagiosConfig = NagiosConfig {_nagiosPluginsPath :: FilePath}
                        deriving (Show, Eq, Typeable)

instance Default NagiosConfig where
    def = NagiosConfig "/usr/lib/nagios/plugins/"

makeFields ''NagiosConfig

extractConfig' :: (ConfigReader m) => m NagiosConfig
extractConfig' = extractConfig $ (pluginDef def)^.name

-- | Provides the PluginDef for the Nagios plugin. Provide this to
-- 'addPlugin' in the 'registerPlugins' block in your app config/main.
-- Provide a NagiosConfig record - use 'def' for default values
--
-- > addPlugin $ Nagios.pluginDef def { _nagiosPluginPath = ... }
pluginDef :: NagiosConfig -> PluginDef
pluginDef conf = definePlugin "Nagios" conf $ do
    register (actionType :: Command)
    register (actionType :: CheckTCP)
    register (actionType :: CommandX)

-- | Supported Actions
data Command = Command { _commandHost :: Text
                       , _commandPort :: Maybe Int
                       , _commandShellCommand :: Text
                       } deriving (Show, Eq, Typeable)

data CheckTCP = CheckTCP { _checktcpHost :: Text
                         , _checktcpPort :: Int
                         } deriving (Show, Eq, Typeable)

data CommandX = SeeItsExistentialBro Int deriving (Show, Eq, Typeable)


makeFields ''Command
deriveSafeCopy 1 'base ''Command

instance Cereal.Serialize Command where
    put = safePut
    get = safeGet

instance Stashable Command where
    key cmd = fromT $ cmd^.host

data NagiosResult = OK Text | Warning Text | Critical Text | Unknown Text
    deriving (Show, Eq, Typeable)

instance Resulting NagiosResult

deriveBinary ''NagiosResult
deriveSafeCopy 1 'base ''NagiosResult
instance Cereal.Serialize NagiosResult where
    put = safePut
    get = safeGet

instance Stashable NagiosResult where
    key = error "NagiosResult not really stashable yet"

instance Runnable Command NagiosResult where
    exec cmd =
        catchAny (
             do cmdPath <- commandPath
                (rc, result, _) <- liftIO $ readProcessWithExitCode cmdPath makeArgs []
                return $ case rc of
                    ExitSuccess   -> completeAs OK result
                    ExitFailure 1 -> completeAs Warning result
                    ExitFailure 2 -> completeAs Critical result
                    ExitFailure i -> Left $ tshow i ++ ": " ++ toT result )
             (\ exception -> do
                putStrLn $ "Command exec threw exception: " ++ tshow exception
                return $ Left $ tshow exception )
      where
        makeArgs = ["-H", fromT $ cmd^.host, "-p", portS $ cmd^.port]
        portS (Just p) = showStr p
        portS Nothing = ""
        completeAs :: (Text -> NagiosResult) -> String -> Either Text NagiosResult
        completeAs f result = Right $ f $ toT result
        commandPath = do
            nagconf <- extractConfig'
            let cmdPath = (nagconf^.pluginsPath) </> fromT (cmd^.shellCommand)
            return $ fpToString cmdPath


deriveSafeCopy 1 'base ''CommandX

instance Cereal.Serialize CommandX where
    put = safePut
    get = safeGet

instance Stashable CommandX where
    key _ = undefined

instance Runnable CommandX NagiosResult where
    exec _ = undefined


makeFields ''CheckTCP
deriveSafeCopy 1 'base ''CheckTCP

instance Cereal.Serialize CheckTCP where
    put = safePut
    get = safeGet

instance Stashable CheckTCP where
    key c = fromT $ c^.host ++ ":" ++ tshow (c^.port)

instance Runnable CheckTCP NagiosResult where
    exec cmd = exec $ Command (cmd^.host) (Just $ cmd^.port) "check_tcp"
