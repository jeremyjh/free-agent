{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Dash.Store
    ( fetch
    , stash, stashWrapped
    , Stashable(..)
    , get, put
    , scan, ScanQuery(..), queryItems, queryList, queryBegins
    , Key, Value, KeySpace
    , runLevelDB, LevelDB, withKeySpace
    ) where

import           Dash.Prelude
import           Database.LevelDB.Higher
import           Dash.Proto
        (ProtoBuf(..), wrap, ProtoFail(..),encodeRaw)


-- | Types that can be serialized, stored and retrieved by Dash
--
-- A 'ProtoBuf' with a key
class (ProtoBuf a) => Stashable a where
    key :: a -> Key

-- | Store the ProtoBuf in the database
--
stash :: (Stashable s) => s -> LevelDB ()
stash s = put (key s) (toStrict $ encode s)

-- | Fetch the ProtoBuf from the database
--
fetch :: (ProtoBuf a) => Key -> LevelDB (Either ProtoFail a)
fetch k = map decode_found $ get k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decode $ toLazy bs

-- | Wrap and store the ProtoBuf in the database
--
stashWrapped :: (Stashable s) => s -> LevelDB ()
stashWrapped s = put (key s) (toStrict $ encodeRaw $ wrap s)
