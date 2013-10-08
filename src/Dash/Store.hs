{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Dash.Store
    ( fetch, scanFetch
    , stash,  stashB, store
    , decodeStore
    , FetchFail(..), Storeable, Stashable(..)
    , get, put, putB
    , scan, ScanQuery(..), queryItems, queryList, queryBegins
    , Key, Value, KeySpace
    , runLevelDB, runCreateLevelDB, LevelDB, withKeySpace, runBatch
    ) where

import           Dash.Prelude
import qualified Prelude as P
import qualified Data.ByteString.Char8 as BS
import           Control.Monad.Writer
import           Database.LevelDB.Higher
import           Data.Serialize           hiding (get, put)
import qualified Data.Serialize           as Cereal
import           Data.SafeCopy


data FetchFail = ParseFail String | NotFound String deriving (Show, Eq)

type Storeable a = (Serialize a, Show a, Typeable a)

-- | Types that can be serialized, stored and retrieved
--
class (Storeable a) => Stashable a where
    key :: a -> Key


decodeStore :: (Serialize a) => ByteString -> Either FetchFail a
decodeStore serial =
    case decode serial of
    Left s -> Left $ ParseFail s
    Right ser -> Right ser

-- | Save a serializeble type using a provided key
store :: (MonadLevelDB m, Serialize s) => Key -> s -> m ()
store k s = put k (encode s)

-- | Save a serailizable type with an instance for Stash
-- which provides the key.
--
stash :: (MonadLevelDB m, Stashable s) => s -> m ()
stash s = store (key s) s

-- | Store the 'Stashable' in the database - batch mode with 'runBatch'
--
storeB :: (MonadLevelDB m, Stashable s)
       => Key
       -> s -> WriterT WriteBatch m ()
storeB k s = putB k (encode s)

-- | Store the 'Stashable' in the database - batch mode with 'runBatch'
--
stashB :: (MonadLevelDB m, Stashable s) => s -> WriterT WriteBatch m ()
stashB s = storeB (key s) s

-- | Fetch the 'Stashable' from the database
--
fetch :: (MonadLevelDB m, Storeable a) => Key -> m (Either FetchFail a)
fetch k = map decode_found $ get k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decodeStore bs

scanFetch :: (MonadLevelDB m, Stashable a) => Key -> m [Either FetchFail a]
scanFetch k = scan k queryList {scanMap = \ (_, v) -> decodeStore v}
