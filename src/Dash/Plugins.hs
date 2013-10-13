{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Dash.Plugins where

import           Dash.Prelude
import           Dash.Store
import           Dash.Types
import qualified Dash.Plugins.Nagios     as NS
import           Data.Serialize          (decode)

import qualified Data.ByteString.Char8   as BS


-- | Each plugin should expose a registerUnWrappers with the same signature
--
registerUnWrappers :: [PluginUnWrapper Action]
registerUnWrappers = NS.registerUnWrappers -- ++ ..

-- | Deserializes and unWraps the underlying type using
-- the 'registerUnWrappers' defined for it
decodeAction :: ByteString -> FetchAction
decodeAction bs = tryDecode (decode bs) >>= pluginUnWrapper
  where
    tryDecode (Left s) = Left $ ParseFail s
    tryDecode (Right w) = Right w

-- | Like Store.Fetch for an action using 'decodeAction' to deserialize
--
fetchAction :: (MonadLevelDB m) => Key -> m FetchAction
fetchAction k = map decode_found $ get k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decodeAction bs

pluginUnWrapper :: Wrapped -> FetchAction
pluginUnWrapper wrapper =
    found findUnWrap wrapper
  where
    findUnWrap =
        find (\(n, _) -> fiName == n) registerUnWrappers
    found (Just uwf) = snd uwf
    found Nothing =
        error $ "Type Name: " ++ BS.unpack fiName ++ " not matched! Is your plugin registered?"
    fiName = wType wrapper
