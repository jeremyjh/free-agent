{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Dash.Action (decodeAction, fetchAction) where

import           Dash.Prelude
import           Dash.Types
import           Dash.Store
import           Dash.Plugins   (pluginUnWrapper)
import           Data.Serialize (decode)

-- | Deserializes and unWraps the underlying type using
-- the 'registerUnWrappers' defined for it
decodeAction :: ByteString -> Either FetchFail (Action a)
decodeAction bs = cerResult (decode bs) >>= pluginUnWrapper
  where
    cerResult (Left s) = Left $ ParseFail s
    cerResult (Right w) = Right w

-- | Like Store.Fetch for an action using 'decodeAction' to deserialize
--
fetchAction :: Key -> LevelDB (Either FetchFail (Action a))
fetchAction k = map decode_found $ get k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decodeAction bs
