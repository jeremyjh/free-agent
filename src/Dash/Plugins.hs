{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Dash.Plugins where

import           Dash.Prelude
import           Dash.Store
import           Dash.Types
import qualified Dash.Plugins.Nagios               as NS

import qualified Data.ByteString.Char8             as BS


-- | Each plugin should expose a registerUnWrappers with the same signature
--
registerUnWrappers :: [(ByteString, Wrapper -> Either FetchFail (Action a))]
registerUnWrappers = NS.registerUnWrappers -- ++ ..

pluginUnWrapper :: Wrapper -> Either FetchFail (Action a)
pluginUnWrapper wrapper =
    found findUnWrap wrapper
  where
    findUnWrap =
        find (\(n, _) -> fiName == n) registerUnWrappers
    found (Just uwf) = snd uwf
    found Nothing =
        error $ "Type Name: " ++ BS.unpack fiName ++ " not matched! Is your plugin registered?"
    fiName = wType wrapper
