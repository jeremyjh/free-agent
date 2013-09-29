{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Dash.Store
    ( fetch, scanFetch
    , stash, stashWrapped, stashB
    , Stashable(..)
    , get, put, putB
    , scan, ScanQuery(..), queryItems, queryList, queryBegins
    , Key, Value, KeySpace
    , runLevelDB, runCreateLevelDB, LevelDB, withKeySpace, runBatch
    ) where

import           Dash.Prelude
import           Control.Monad.Writer
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

-- | Store the ProtoBuf in the database - batch mode with 'runBatch'
--
stashB :: (MonadLevelDB m, Stashable s) => s -> WriterT WriteBatch m ()
stashB s = putB (key s) (toStrict $ encode s)

-- | Fetch the ProtoBuf from the database
--
fetch :: (MonadLevelDB m, ProtoBuf a) => Key -> m (Either ProtoFail a)
fetch k = map decode_found $ get k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decode $ toLazy bs

scanFetch :: (MonadLevelDB m, ProtoBuf a) => Key -> m [Either ProtoFail a]
scanFetch k = scan k queryList {scanMap = \ (_, v) -> decode $ toLazy v}

-- | Wrap and store the ProtoBuf in the database
--
stashWrapped :: (Stashable s) => s -> LevelDB ()
stashWrapped s = put (key s) (toStrict $ encodeRaw $ wrap s)
