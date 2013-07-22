{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.Store
    ( fetch
    , stash
    , get, put
    , openDB, DB
    , Key(..)
    , Stashable(..)
    ) where

import           BasicPrelude
import qualified Prelude as P
import           Control.Monad
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Resource      (release, ResIO)
import           Data.ByteString.Char8             (pack)
import           Data.Default                      (def)

import qualified Database.LevelDB                  as LDB

import           Dash.Proto                        (ProtoBuf(..), wrap
                                                   ,toStrict, toLazy, Wrapper(..))


-- | Key provided in up to four parts.
--
-- A simple, efficient scheme to allow scans of partial keys; structure data up to three layers, for example:
-- ("accounts","domestic", "petroleum", "CustID-3387020-2") - this would enable you to do efficient queries
-- to retrieve all accounts, all domestic accounts, all domestric petroleum accounts etc.
data Key = Key ByteString | KeyPair (ByteString, ByteString)
         | KeyTri (ByteString, ByteString, ByteString)
         | KeyQuad (ByteString,ByteString,ByteString, ByteString)

instance IsString Key where fromString = Key . pack

hashKey :: Key -> ByteString
hashKey (Key k) = k
hashKey (KeyPair (k1, k2)) = k1 ++ k2
hashKey (KeyTri (k1, k2, k3)) = k1 ++ k2 ++ k3
hashKey (KeyQuad (k1, k2, k3, k4)) = k1 ++ k2 ++ k3 ++ k4

type DB = LDB.DB

-- | Types that can be serialized, stored and retrieved by Dash
--
class (ProtoBuf a) => Stashable a where
    key :: a -> Key

stash :: Stashable s => DB -> s -> ResIO ()
stash db s = put db (key s) (toStrict $ encode s)

fetch :: (Stashable s) => DB -> Key -> ResIO s
fetch db k = liftM decode_found $ get db k
  where
    decode_found Nothing = error "Didn't find value for key"
    decode_found (Just bs) = decode $ toLazy bs

put :: DB -> Key -> ByteString -> ResIO ()
put db k = LDB.put db def $ hashKey k

get :: DB -> Key -> ResIO (Maybe ByteString)
get db k = LDB.get db def $ hashKey k

openDB :: P.FilePath -> ResIO DB
openDB path =  LDB.open path
    LDB.defaultOptions{LDB.createIfMissing = True, LDB.cacheSize= 2048}
