{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dash.Plugins(pluginUnWrapper) where

import           Dash.Prelude
import           Dash.Store                        (Wrapper(..), StashFail)
import {-# SOURCE #-}
                 Dash.Action                       (Action(..))
import qualified Dash.Plugins.Nagios               as NS

import qualified Data.ByteString.Char8             as BS


-- | Each plugin should expose a registerUnWrappers with the same signature
--
registerUnWrappers :: [(ByteString, Wrapper -> Either StashFail (Action a))]
registerUnWrappers = NS.registerUnWrappers -- ++ ..

pluginUnWrapper :: Wrapper -> Either StashFail (Action a)
pluginUnWrapper wrapper =
    found findUnWrap wrapper
  where
    findUnWrap =
        find (\(n, _) -> fiName == n) registerUnWrappers
    found (Just uwf) = snd uwf
    found Nothing =
        error $ "Type Name: " ++ BS.unpack fiName ++ " not matched! Is your plugin registered?"
    fiName = wType wrapper
