{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Dash.Store
    ( fetch
    , stash, stashWrapped
    , get, put, scan, scanBegins, scanBeginsMap, scanBeginsMapFilter
    , DashDB, runDashDB, withKeySpace
    , Key, KeySpace
    , Stashable(..)
    ) where

import           Dash.Prelude
import qualified Prelude as P
import           Control.Monad.Trans.Resource
        (ResourceT, ResIO, liftResourceT, runResourceT
        ,MonadResource, MonadUnsafeIO, MonadThrow)

import           Control.Monad.Reader
        (ReaderT, runReaderT, local, asks, MonadReader)


import           Control.Monad.Base               (MonadBase)
import           Control.Concurrent.Lifted        (fork)
import           Control.Concurrent.MVar.Lifted
        (MVar, newMVar, takeMVar, putMVar)

import qualified Data.ByteString                   as BS hiding (pack)
import qualified Data.Binary                       as Binary
import           Data.Default                      (def)

import qualified Database.LevelDB                  as LDB
import           Database.LevelDB                  hiding (put, get)

import           Data.Maybe.Utils                  (forceMaybe)

import           Dash.Proto
        (ProtoBuf(..), wrap, ProtoFail(..),encodeRaw)

type Key = ByteString
type Value = ByteString
type KeySpace = ByteString
type KeySpaceId = ByteString
type Item = (Key, Value)

-- | Reader-based data context API
--
-- Context contains database handle and KeySpace
data DBContext = DBC { dbcDb :: DB
                     , dbcKsId :: KeySpaceId
                     , dbcSyncMV :: MVar Int32
                     }
instance Show (DBContext) where
    show = (++) "KeySpaceID: " . P.show . dbcKsId

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

instance Show (DashDB a) where
    show = asks P.show

-- | Specify a filepath to use for the database (will create if not there)
-- Also specify an application-defined keyspace in which keys will be guaranteed unique
runDashDB :: FilePathS -> KeySpace -> DashDB a -> IO a
runDashDB dbPath ks ctx = runResourceT $ do
    db <- openDB dbPath
    mv <- newMVar 0
    ksId <- withSystemContext db mv $ getKeySpaceId ks
    runReaderT (unDBCIO ctx) (mkDBC db ksId mv)
  where
    openDB path =
        LDB.open path
            LDB.defaultOptions{LDB.createIfMissing = True, LDB.cacheSize= 2048}
    withSystemContext db mv sctx =
        runReaderT (unDBCIO sctx) $ mkDBC db systemKeySpaceId mv

-- | Override keyspace with a local keyspace for an (block) action(s)
--
withKeySpace :: KeySpace -> DashDB a -> DashDB a
withKeySpace ks a = do
    ksId <- getKeySpaceId ks
    local (\dbc -> dbc { dbcKsId = ksId}) a

put :: Key -> Value -> DashDB ()
put k v = do
    (db, ksId) <- asks $ dbcDb &&& dbcKsId
    let packed = ksId ++ k
    liftResourceT $ LDB.put db def packed v

get :: Key -> DashDB (Maybe Value)
get k = do
    (db, ksId) <- asks $ dbcDb &&& dbcKsId
    let packed = ksId ++ k
    liftResourceT $ LDB.get db def packed

-- | Find and return sorted List of items where the key begins with the supplied parameter
scanBegins :: Key -> DashDB [Item]
scanBegins k = scanBeginsMapFilter k id (\_ -> True)

-- | Find and return sorted List of items where the key begins with the supplied parameter
--
-- Use provided functiont to map or transform the Item before pre-pending it to result
scanBeginsMap :: Key
              -> (Item -> Item)  -- ^ map or transform an item
              -> DashDB [Item]
scanBeginsMap k mapFn = scanBeginsMapFilter k mapFn (\_ -> True)

-- | Find and return sorted List of items where the key begins with the supplied parameter
scanBeginsMapFilter :: Key
              -> (Item -> Item)  -- ^ map or transform an item
              -> (Item -> Bool) -- ^ filter function - 'False' to leave this 'Item' out of the list
              -> DashDB [Item]
scanBeginsMapFilter prefix mapFn filterFn = scan prefix beginsPrefix mapFn filterFn
  where
    beginsPrefix (nk, _) _ = nk >= prefix && nk < addOne(prefix)
    addOne bs = BS.snoc (BS.take (BS.length bs - 1) bs) $ BS.last bs + 1

-- | Scan the keyspace, applying functions and returning results
--
-- Look at the 'scanBegins' functions first to see if those will do what you need
scan :: Key  -- ^ Key at which to start the scan
     -> (Item -> [Item] -> Bool) -- ^ scan will continue until this returns false
     -> (Item -> Item)  -- ^ map or transform an item before its prepended to the accumulator
     -> (Item -> Bool) -- ^ filter function - 'False' to leave this 'Item' out of the list
     -> DashDB [Item]
scan k contFn mapFn filterFn = do
    (db, ksId) <- asks $ dbcDb &&& dbcKsId
    liftResourceT $ withIterator db def $ doScan (ksId ++ k)
  where
    doScan :: Key -> Iterator -> ResIO [Item]
    doScan prefix iter = do
        iterSeek iter prefix
        applyIterate []
      where
        applyIterate :: [Item] -> ResIO [Item]
        applyIterate acc = do
            mk <- iterKey iter
            mv <- iterValue iter
            case (mk, mv) of
                (Just nk, Just nv) ->
                    let unSpaced = BS.drop 4 nk in
                    if (contFn (unSpaced, nv) acc) then do
                        iterNext iter
                        items <- applyIterate acc
                        if filterFn (nk, nv) then
                            return $ mapFn (unSpaced, nv) : items
                        else return items
                    else return acc
                _ -> return acc


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

defaultKeySpaceId :: KeySpaceId
defaultKeySpaceId = "\0\0\0\0"

systemKeySpaceId ::  KeySpaceId
systemKeySpaceId = "\0\0\0\1"

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
            0 -> initKeySpaceIdMV k >> takeMVarDBC
            n -> return n
        let nextId = curId + 1
        put k (toBS nextId)
        putMVarDBC nextId
        return $ toBS curId
    initKeySpaceIdMV k = do
        findMaxId <- get k
        case findMaxId of
            (Just found) -> putMVarDBC $ toInt32 found
            Nothing      -> putMVarDBC 2 -- first user keyspace
    putMVarDBC v = asks dbcSyncMV >>= flip putMVar v
    takeMVarDBC = asks dbcSyncMV >>= takeMVar
    toInt32 bs = (Binary.decode $ toLazy bs) :: Int32
    toBS = toStrict . Binary.encode
