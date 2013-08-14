{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.Store
    ( fetch
    , stash, stashWrapped
    , get, put
    , DBContextIO, withDBContext, withKeySpace
    , Key, KeySpace
    , Stashable(..)
    ) where

import           Dash.Prelude
import           Control.Monad.Trans.Resource      (ResourceT, ResIO, liftResourceT, runResourceT)
import           Control.Monad.Trans.Reader

import qualified Data.ByteString                   as BS
import           Data.Default                      (def)

import qualified Database.LevelDB                  as LDB

import           Dash.Proto                        (ProtoBuf(..), wrap
                                                   ,ProtoFail(..)
                                                   ,encodeRaw)
-- | Key provided in up to four parts.
--
type Key = ByteString
type KeySpace = ByteString

-- | Lookup the KeySpace ID and pre-pend it to the Key
packKey :: Key -> KeySpace -> ByteString
packKey k ks = BS.take 4 ks ++ k

type DB = LDB.DB

-- | Types that can be serialized, stored and retrieved by Dash
-- A ProtoBuf with a key
class (ProtoBuf a) => Stashable a where
    key :: a -> Key

openDB :: FilePathS -> ResIO DB
openDB path =
    LDB.open path
        LDB.defaultOptions{LDB.createIfMissing = True, LDB.cacheSize= 2048}

-- | Reader-based data context API
--
-- Context contains database handle and KeySpace
data DBContext = DBC DB ByteString
type DBContextIO a = ReaderT DBContext (ResourceT IO) a

setKeySpace :: ByteString -> DBContext -> DBContext
setKeySpace ks (DBC db _) = DBC db ks

-- | Specify a filepath to use for the database (will create if not there)
-- Also specify a keyspace in which keys will be guaranteed unique
withDBContext :: FilePathS -> ByteString -> DBContextIO a -> IO a
withDBContext dbPath ks ctx = runResourceT $ do
    db <- openDB dbPath
    runReaderT ctx $ DBC db ks

-- | Override keyspace with a local keyspace for an (block) action(s)
--
withKeySpace :: ByteString -> DBContextIO a -> DBContextIO a
withKeySpace ks = local (setKeySpace ks)

put :: Key -> ByteString -> DBContextIO ()
put k v = do
    DBC db ks <- ask
    let pk = packKey k ks
    liftResourceT $ LDB.put db def pk v

get :: Key -> DBContextIO (Maybe ByteString)
get k = do
    DBC db ks <- ask
    let pk = packKey k ks
    liftResourceT $ LDB.get db def pk

-- | Store the ProtoBuf in the database
--
stash :: (Stashable s) => s -> DBContextIO ()
stash s = put (key s) (toStrict $ encode s)

-- | Fetch the ProtoBuf from the database
--
fetch :: (ProtoBuf a) => Key -> DBContextIO (Either ProtoFail a)
fetch k = map decode_found $ get k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decode $ toLazy bs

-- | Wrap and store the ProtoBuf in the database
--
stashWrapped :: (Stashable s) => s -> DBContextIO ()
stashWrapped s =
    put (key s) (toStrict $ encodeRaw $ wrap s)
