{-# LANGUAGE NoImplicitPrelude, OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dash.Plugins.Nagios(Command(..), registerActions) where

import           Dash.Prelude
import           Dash.Types
import           Dash.Action

import           System.Process                    (readProcess)
import           Data.Serialize                    as Cereal
import           Data.SafeCopy

data Command = Command { host :: Text
                       , port :: Maybe Int
                       , command :: Text
                       } deriving (Show, Eq, Typeable)

deriveSafeCopy 1 'base ''Command

instance Serialize Command where
    put = safePut
    get = safeGet

instance Stashable Command where
    key = fromT . host

instance Runnable Command where
    exec cmd =
        readProcess (fromT $ command cmd) (makeArgs cmd) []
            >> return (Complete $ Just "Awesome")
      where
        makeArgs c = ["-H", fromT $ host c, "-p", portS $ port c]
        portS (Just p) = showStr p
        portS Nothing = ""

registerActions :: PluginWriter
registerActions = do
    register (actionType :: Command)
    register (actionType :: CommandX)

data CommandX = SeeItsExistentialBro Int deriving (Show, Eq, Typeable)

deriveSafeCopy 1 'base ''CommandX

instance Serialize CommandX where
    put = safePut
    get = safeGet

instance Stashable CommandX where
    key _ = error "classy deprecates undefined"

instance Runnable CommandX where
    exec _ = error "classy deprecates undefined"
