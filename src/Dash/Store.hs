{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Dash.Store
    ( fetch
    , stash, stashWrapped
    , get, put
    , DashDB, runDashDB, withKeySpace
    , Key, KeySpace
    , Stashable(..)
    ) where

import           Dash.Prelude
import           Control.Monad.Trans.Resource
        (ResourceT, ResIO, liftResourceT, runResourceT
        ,MonadResource, MonadUnsafeIO, MonadThrow)

import           Control.Monad.Reader
        (ReaderT, runReaderT, local, asks, MonadReader)


import           Control.Monad.Base               (MonadBase)
import           Control.Concurrent.Lifted        (fork)
import           Control.Concurrent.MVar.Lifted
        (MVar, newMVar, takeMVar, putMVar)

import qualified Data.ByteString.Char8             as BS
import qualified Data.ByteString                   as BS hiding (pack)
import qualified Data.Binary                       as Binary
import           Data.Default                      (def)

import qualified Database.LevelDB                  as LDB

import           Dash.Proto
        (ProtoBuf(..), wrap, ProtoFail(..),encodeRaw)

type Key = ByteString
type KeySpace = ByteString
type KeySpaceId = ByteString
type DB = LDB.DB

-- | Reader-based data context API
--
-- Context contains database handle and KeySpace
data DBContext = DBC { dbcDb :: DB
                     , dbcKsId :: KeySpaceId
                     , dbcSyncMV :: MVar Int32
                     }
mkDBC :: DB -> KeySpaceId -> MVar Int32 -> DBContext
mkDBC db ksId mv = DBC {dbcDb = db
                       ,dbcKsId = ksId
                       ,dbcSyncMV = mv
                       }

-- | DashDB Monad provides a context for database operations provided in this module
--
-- Use runDashDB
newtype DashDB a = DBCIO {unDBCIO :: ReaderT DBContext (ResourceT IO) a }
    deriving ( Functor, Applicative, Monad
             , MonadIO, MonadBase IO, MonadReader DBContext
             , MonadResource, MonadUnsafeIO, MonadThrow )

-- | Specify a filepath to use for the database (will create if not there)
-- Also specify an applicatin-defined keyspace in which keys will be guaranteed unique
runDashDB :: FilePathS -> KeySpace -> DashDB a -> IO a
runDashDB dbPath ks ctx = runResourceT $ do
    db <- openDB dbPath
    mv <- (newMVar 0)
    ksId <- withSystemContext db mv $ getKeySpaceId ks
    runReaderT (unDBCIO ctx) (mkDBC db ksId mv)

-- | Override keyspace with a local keyspace for an (block) action(s)
--
withKeySpace :: KeySpace -> DashDB a -> DashDB a
withKeySpace ks a = do
    ksId <- getKeySpaceId ks
    local (\dbc -> dbc { dbcKsId = ksId}) a

put :: Key -> ByteString -> DashDB ()
put k v = do
    (db, ks) <- getDBKS
    let pk = packKey k ks
    liftResourceT $ LDB.put db def pk v

get :: Key -> DashDB (Maybe ByteString)
get k = do
    (db, ks) <- getDBKS
    let pk = packKey k ks
    liftResourceT $ LDB.get db def pk

-- | Types that can be serialized, stored and retrieved by Dash
-- A ProtoBuf with a key
class (ProtoBuf a) => Stashable a where
    key :: a -> Key

-- | Store the ProtoBuf in the database
--
stash :: (Stashable s) => s -> DashDB ()
stash s = put (key s) (toStrict $ encode s)

-- | Fetch the ProtoBuf from the database
--
fetch :: (ProtoBuf a) => Key -> DashDB (Either ProtoFail a)
fetch k = map decode_found $ get k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decode $ toLazy bs

-- | Wrap and store the ProtoBuf in the database
--
stashWrapped :: (Stashable s) => s -> DashDB ()
stashWrapped s = put (key s) (toStrict $ encodeRaw $ wrap s)

defaultKeySpaceId :: ByteString
defaultKeySpaceId = "\0\0\0\0"

systemKeySpaceId :: ByteString
systemKeySpaceId = "\0\0\0\1"

withSystemContext :: DB -> MVar Int32 -> DashDB a -> ResIO a
withSystemContext db mv ctx =
    runReaderT (unDBCIO ctx) $
               mkDBC db systemKeySpaceId mv

getKeySpaceId :: KeySpace -> DashDB KeySpaceId
getKeySpaceId ks
    | ks == ""  = return defaultKeySpaceId
    | ks == "system" = return systemKeySpaceId
    | otherwise = do
        findKS <- get $ "keyspace:" ++ ks
        case findKS of
            (Just foundId) -> return foundId
            Nothing -> do -- define new KS
                nextId <- incr "max-keyspace-id"
                put ("keyspace:" ++ ks) nextId
                return nextId
  where
    incr k = do
        mv <- takeMVarDBC
        curId <- case mv of
            0 -> initKeySpaceIdM k >> takeMVarDBC
            n -> return n
        let nextId = curId + 1
        put k (toBS nextId)
        putMVarDBC nextId
        return $ toBS curId

    initKeySpaceIdM k = do
        findMaxId <- get k
        case findMaxId of
            (Just found) -> do
                putMVarDBC $ toInt32 found
            Nothing -> do --initialize
                putMVarDBC 2 -- first user keyspace

    putMVarDBC v = (asks dbcSyncMV) >>= flip putMVar v
    takeMVarDBC = (asks dbcSyncMV) >>= takeMVar
    toInt32 bs = (Binary.decode $ toLazy bs) :: Int32
    toBS = toStrict . Binary.encode

-- | Lookup the KeySpace ID and pre-pend it to the Key
packKey :: Key -> KeySpace -> ByteString
packKey k ksId = ksId ++ k

openDB :: FilePathS -> ResIO DB
openDB path =
    LDB.open path
        LDB.defaultOptions{LDB.createIfMissing = True, LDB.cacheSize= 2048}

getDBKS :: DashDB (DB, KeySpaceId)
getDBKS = asks (\d -> (dbcDb d, dbcKsId d))
