{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dash.Plugins(pluginUnWrapper) where

import           BasicPrelude
import qualified Prelude                           as P
import           System.Process                    (readProcess)

import           Dash.Proto
import           Dash.Store                        (Stashable(..), Key(..))
import           Dash.Runner                       (Runnable(..), RunStatus(..))
import {-# SOURCE #-}
                 Dash.Action                       (Action(..))

import qualified Dash.Plugins.Nagios.Proto.Command as NC



pluginUnWrapper :: Wrapper -> Action a
pluginUnWrapper wrapper =
    Action $ findUnWrap wrapper
  where
    fiName = typeName wrapper
    findUnWrap
        | fiName == ".dash.plugins.nagios.proto.Command" =
            unWrap :: Wrapper -> NC.Command
        | otherwise =
            error "FIName not matched! Is your plugin registered?"

instance ProtoBuf NC.Command
instance Stashable NC.Command where
    key cmd = Key $ toStrict $ utf8 $ NC.host cmd

instance Runnable NC.Command where
    exec cmd =
        readProcess (uToString $ NC.command cmd) (makeArgs cmd) []
            >> return (Complete $ Just "Awesome")
      where
        makeArgs c = ["-H", uToString $ NC.host c, "-p", port $ NC.port c]
        port (Just p) = P.show p
        port Nothing = ""
