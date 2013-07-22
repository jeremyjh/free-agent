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

import qualified Dash.Proto.Runnable.NagiosCommand as NC



pluginUnWrapper :: Utf8 -> (Wrapper -> Action a)
pluginUnWrapper fiName =
    Action . unWrapFn
  where
    unWrapFn
        | fiName == ".dash.proto.runnable.NagiosCommand" =
            (\w -> unWrap w ) :: (Wrapper -> NC.NagiosCommand)
        | otherwise =
            error "FIName not matched! Is your plugin registered?"

instance ProtoBuf NC.NagiosCommand
instance Stashable NC.NagiosCommand where
    key cmd = Key $ toStrict $ utf8 $ NC.host cmd

instance Runnable NC.NagiosCommand where
    exec cmd =
        readProcess (uToString $ NC.command cmd) (makeArgs cmd) []
            >> return (Complete $ Just "Awesome")
      where
        makeArgs c = ["-H", uToString $ NC.host c, "-p", port $ NC.port c]
        port (Just p) = P.show p
        port Nothing = ""
