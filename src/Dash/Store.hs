{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Dash.Store
    ( fetch, scanFetch
    , stash, stashWrapped, stashB, store
    , wrap, unWrap, fqName, decodeStore
    , StashFail(..), Storeable, Stashable(..), Wrapper(..)
    , get, put, putB
    , scan, ScanQuery(..), queryItems, queryList, queryBegins
    , Key, Value, KeySpace
    , runLevelDB, runCreateLevelDB, LevelDB, withKeySpace, runBatch
    ) where

import           Dash.Prelude
import qualified Prelude as P
import qualified Data.ByteString.Char8 as BS
import           Data.Typeable
import           Control.Monad.Writer
import           Database.LevelDB.Higher
import           Data.Serialize           hiding (get, put)
import qualified Data.Serialize           as Cereal
import           Data.SafeCopy


data StashFail = BytesLeftover | ParseFail String | NotFound String deriving (Show, Eq)

type Storeable a = (Serialize a, Show a, Typeable a)

-- | Types that can be serialized, stored and retrieved
--
class (Storeable a) => Stashable a where
    key :: a -> Key

data Wrapper = Wrapper { wType :: ByteString
                       , value :: ByteString }
                deriving (Show, Typeable)

deriveSafeCopy 1 'base ''Wrapper

instance Serialize Wrapper where
    get = safeGet
    put = safePut

decodeStore :: (Serialize a) => ByteString -> Either StashFail a
decodeStore serial =
    case decode serial of
    Left s -> Left $ ParseFail s
    Right ser -> Right ser

store :: (MonadLevelDB m, Stashable s) => s -> m ()
store s = put (key s) (encode s)
-- | Store the 'Stashable' in the database
--
stash :: (MonadLevelDB m, Stashable s) => s -> m ()
stash s = put (key s) (encode s)

-- | Store the 'Stashable' in the database - batch mode with 'runBatch'
--
stashB :: (MonadLevelDB m, Stashable s) => s -> WriterT WriteBatch m ()
stashB s = putB (key s) (encode s)

-- | Fetch the 'Stashable' from the database
--
fetch :: (MonadLevelDB m, Storeable a) => Key -> m (Either StashFail a)
fetch k = map decode_found $ get k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decodeStore bs

scanFetch :: (MonadLevelDB m, Stashable a) => Key -> m [Either StashFail a]
scanFetch k = scan k queryList {scanMap = \ (_, v) -> decodeStore v}

-- | Wrap and store the 'Stashable' in the database
--
stashWrapped :: (Stashable a) => a -> LevelDB ()
stashWrapped s = put (key s) (encode $ wrap s)

wrap :: (Storeable a) => a -> Wrapper
wrap st = Wrapper (fqName st) (encode st)

unWrap :: (Stashable a) => Wrapper -> Either StashFail a
unWrap = decodeStore . value

fqName :: (Typeable a) => a -> ByteString
fqName typee =  modName ++ "." ++ typeName
  where
    typeName = BS.pack $ P.show $ typeOf typee
    modName = BS.pack $ tyConModule $ typeRepTyCon $ typeOf typee
