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
import qualified Data.ByteString                   as BS (replicate)
import           Data.Default                      (def)

import qualified Crypto.Hash.SHA1                  as SHA1

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



-- | Key is up to 4-tuple, padded w/ 0s to become 80 bytes for key scans
--
hashKey :: Key -> ByteString
hashKey (Key k) = sha1 k ++ pad 3
hashKey (KeyPair (k1, k2)) = sha1 k1 ++ sha1 k2 ++ pad 2
hashKey (KeyTri (k1, k2, k3)) = sha1 k1 ++ sha1 k2 ++ sha1 k3 ++ pad 1
hashKey (KeyQuad (k1, k2, k3, k4)) = sha1 k1 ++ sha1 k2 ++ sha1 k3 ++ sha1 k4

sha1 :: ByteString -> ByteString
sha1 = SHA1.hash

pad :: Int -> ByteString
pad = flip BS.replicate 0 . (*20)

type DB = LDB.DB

-- | Types that can be serialized, stored and retrieved by Dash
--
class (ProtoBuf a) => Stashable a where
    key :: a -> Key

stash :: Stashable s => DB -> s -> ResIO ()
stash db s = put db (key s) (toStrict $ encode s)

fetch :: (Stashable s) => DB -> Key -> ResIO (Either String s)
fetch db k = liftM decode_found $ get db k
  where
    decode_found Nothing = Left "Didn't find value for key"
    decode_found (Just bs) = decode $ toLazy bs

put :: DB -> Key -> ByteString -> ResIO ()
put db k = LDB.put db def $ hashKey k

get :: DB -> Key -> ResIO (Maybe ByteString)
get db k = LDB.get db def $ hashKey k

openDB :: P.FilePath -> ResIO DB
openDB path =  LDB.open path
    LDB.defaultOptions{LDB.createIfMissing = True, LDB.cacheSize= 2048}
