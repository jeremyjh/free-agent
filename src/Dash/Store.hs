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

import qualified Data.ByteString.Char8             as BS
import qualified Data.ByteString                   as BS hiding (pack)
import qualified Data.Binary                       as Binary
import           Data.Default                      (def)

import qualified Database.LevelDB                  as LDB

import           Dash.Proto                        (ProtoBuf(..), wrap
                                                   ,ProtoFail(..)
                                                   ,encodeRaw)
-- | Key provided in up to four parts.
--
type Key = ByteString
type KeySpace = ByteString
type KeySpaceId = ByteString

defaultKeySpaceId :: ByteString
defaultKeySpaceId = "\0\0\0\0"

systemKeySpaceId :: ByteString
systemKeySpaceId = "\0\0\0\1"

getKeySpaceId :: KeySpace -> DBContextIO KeySpaceId
getKeySpaceId ks
    | ks == ""  = return defaultKeySpaceId
    | ks == "system" = return systemKeySpaceId
    | otherwise = do
        findKS <- get $ "keyspace:" ++ ks
        case findKS of
            (Just foundId) -> return foundId
            Nothing -> do -- define new KS
                nextId <- incr "keyspaceid"
                put ("keyspace:" ++ ks) nextId
                return nextId
  where
    -- TODO: Implement transaction or locking for thread safety
    incr k = do
        findMaxId <- get k
        case findMaxId of
            (Just found) -> do
                let nextId = toBS (toInt32 found + 1)
                put k nextId
                return nextId
            Nothing -> do --initialize
                put k systemKeySpaceId
                return "\0\0\0\1"
    toInt32 bs = (Binary.decode $ toLazy bs) :: Int32
    toBS = toStrict . Binary.encode


-- | Lookup the KeySpace ID and pre-pend it to the Key
packKey :: Key -> KeySpace -> ByteString
packKey k ksId = ksId ++ k

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


-- | Specify a filepath to use for the database (will create if not there)
-- Also specify a keyspace in which keys will be guaranteed unique
withDBContext :: FilePathS -> KeySpace -> DBContextIO a -> IO a
withDBContext dbPath ks ctx = runResourceT $ do
    db <- openDB dbPath
    ksId <- withSystemContext db $ getKeySpaceId ks
    runReaderT ctx $ DBC db ksId

withSystemContext :: DB -> DBContextIO a -> ResIO a
withSystemContext db ctx = runReaderT ctx (DBC db systemKeySpaceId)

-- | Override keyspace with a local keyspace for an (block) action(s)
--
withKeySpace :: KeySpace -> DBContextIO a -> DBContextIO a
withKeySpace ks a = do
    ksId <- getKeySpaceId ks
    local (setKeySpace ksId) a

setKeySpace :: KeySpaceId -> DBContext -> DBContext
setKeySpace ksId (DBC db _)= DBC db ksId

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
