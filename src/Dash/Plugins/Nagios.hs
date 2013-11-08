{-# LANGUAGE NoImplicitPrelude, OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dash.Plugins.Nagios
    ( Command(..)
    , CheckTCP(..)
    , registerConfig, registerActions
    ) where

import           Dash.Prelude
import           Dash.Lenses
import           Dash.Action
import           Dash.Core

import           System.Process                    (readProcess)
import           Data.Serialize                    as Cereal
import           Data.SafeCopy

import qualified Data.Map                         as Map
import           Data.Dynamic
import           Data.Default

-- | Plugin-specific configuration
data NagiosConfig = NagiosConfig {_nagiosconfigPluginsPath :: FilePath}
                        deriving (Show, Eq, Typeable)

makeFields ''NagiosConfig

registerConfig :: PluginConfigs
registerConfig = Map.fromList [("Nagios", toDyn defaultNagiosConfig)]

extractConfig' :: (ConfigReader m) => m NagiosConfig
extractConfig' = extractConfig "Nagios"

defaultNagiosConfig :: NagiosConfig
defaultNagiosConfig = NagiosConfig "/usr/lib/nagios/plugins/"

-- | Supported Actions
data Command = Command { _commandHost :: Text
                       , _commandPort :: Maybe Int
                       , _commandShellCommand :: Text
                       } deriving (Show, Eq, Typeable)

data CheckTCP = CheckTCP { _checktcpHost :: Text
                         , _checktcpPort :: Int
                         } deriving (Show, Eq, Typeable)

data CommandX = SeeItsExistentialBro Int deriving (Show, Eq, Typeable)

registerActions :: PluginWriter
registerActions = do
    register (actionType :: Command)
    register (actionType :: CheckTCP)
    register (actionType :: CommandX)


instance Default NagiosConfig where
    def = defaultNagiosConfig


makeFields ''Command
deriveSafeCopy 1 'base ''Command

instance Serialize Command where
    put = safePut
    get = safeGet

instance Stashable Command where
    key cmd = fromT $ cmd^.host

instance Runnable Command where
    exec cmd =
        catchAny (
             do cmdPath <- commandPath
                result <- liftIO $ readProcess cmdPath makeArgs []
                return (Complete $ Just result) )
             (\ exception -> do
                putStrLn $ "Command exec threw exception: " ++ tshow exception
                return $ Failed $ show exception )
      where
        makeArgs = ["-H", fromT $ cmd^.host, "-p", portS $ cmd^.port]
        portS (Just p) = showStr p
        portS Nothing = ""
        commandPath = do
            nagconf <- extractConfig'
            let cmdPath = (nagconf^.pluginsPath) </> fromT (cmd^.shellCommand)
            return $ fpToString cmdPath


deriveSafeCopy 1 'base ''CommandX

instance Serialize CommandX where
    put = safePut
    get = safeGet

instance Stashable CommandX where
    key _ = undefined

instance Runnable CommandX where
    exec _ = undefined


makeFields ''CheckTCP
deriveSafeCopy 1 'base ''CheckTCP

instance Serialize CheckTCP where
    put = safePut
    get = safeGet

instance Stashable CheckTCP where
    key c = fromT $ c^.host ++ ":" ++ tshow (c^.port)

instance Runnable CheckTCP where
    exec cmd = exec $ Command (cmd^.host) (Just $ cmd^.port) "check_tcp"
