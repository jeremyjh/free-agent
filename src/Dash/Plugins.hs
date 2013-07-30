{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dash.Plugins(pluginUnWrapper) where

import           BasicPrelude
import qualified Prelude                           as P
import           System.Process                    (readProcess)

import           Dash.Proto
import {-# SOURCE #-}
                 Dash.Action                       (Action(..))

import qualified Dash.Plugins.Nagios               as NS

-- | Each plugin should expose a registerUnWrappers with the same signature
--
registerUnWrappers :: [(Utf8, Wrapper -> Either ProtoFail (Action a))]
registerUnWrappers = NS.registerUnWrappers -- ++ ..

pluginUnWrapper :: Wrapper -> Either ProtoFail (Action a)
pluginUnWrapper wrapper =
    found findUnWrap $ wrapper
  where
    findUnWrap =
        find (\(n, _) -> fiName == n) registerUnWrappers
    found (Just uwf) = snd uwf
    found Nothing =
        error "FIName not matched! Is your plugin registered?"
    fiName = typeName wrapper
