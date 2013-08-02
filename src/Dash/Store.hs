{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.Store
    ( fetch
    , stash, stashWrapped
    , get, put
    , openDB, DB, ResIO
    , Key(..)
    , Stashable(..)
    ) where

import           Dash.Prelude
import           Control.Monad
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Resource      (release, ResIO)
import           Data.ByteString.Char8             (pack)
import qualified Data.ByteString                   as BS
import           Data.Default                      (def)

import qualified Crypto.Hash.SHA1                  as SHA1

import qualified Database.LevelDB                  as LDB

import           Dash.Proto                        (ProtoBuf(..), wrap
                                                   ,Wrapper(..), ProtoFail(..)
                                                   ,encodeRaw)


-- | Key provided in up to four parts.
--
-- A simple, efficient scheme to allow scans of partial keys; structure data up to three layers, for example:
-- ("accounts","domestic", "petroleum", "CustID-3387020-2") - this would enable you to do efficient queries
-- to retrieve all accounts, all domestic accounts, all domestric petroleum accounts etc.
data Key = Key ByteString
         | Key2 ByteString ByteString
         | Key3 ByteString ByteString ByteString
         | Key4 ByteString ByteString ByteString ByteString
         | KeySpace ByteString Key
         deriving(Show, Eq)

-- | A Key literal can be expressed in the form:
--
-- "keyspace#keypart1:keypart2:keypart3:keypart4"
instance IsString Key where
    fromString keyS =
        case split "#" keyS of
            [ks, rest] -> KeySpace (pack ks) (splitKey rest)
            [rest] -> splitKey rest
            _ -> error "A Key literal can have only one KeySpace #"
      where
        splitKey ks =
            case map pack (split ":" ks) of
                [k] -> Key k
                [k1, k2] -> Key2 k1 k2
                [k1, k2, k3] -> Key3 k1 k2 k3
                [k1, k2, k3, k4] -> Key4 k1 k2 k3 k4
                _ -> error "A key literal can have only 4 Keyparts :"


-- | Key is up to 4-tuple, padded w/ 0s to become 80 bytes for key scans
-- A KeySpace ByteString will be pre-pended to the key and is not hashed
packKey :: Key -> ByteString
packKey pKey =
    case pKey of
        KeySpace bs k -> padSpace bs ++ hashPad k
        k             -> pad 1 ++ hashPad k
  where
    hashPad (Key k) = sha1 k ++ pad 3
    hashPad (Key2 k1 k2) = sha1 k1 ++ sha1 k2 ++ pad 2
    hashPad (Key3 k1 k2 k3) = sha1 k1 ++ sha1 k2 ++ sha1 k3 ++ pad 1
    hashPad (Key4 k1 k2 k3 k4) = sha1 k1 ++ sha1 k2 ++ sha1 k3 ++ sha1 k4
    hashPad (KeySpace _ _) = error "nested KeySpace is not supported"

    sha1 = SHA1.hash
    padSize = 20
    pad = flip BS.replicate 0 . (*padSize)
    padSpace = BS.take padSize . (++ pad 1)

type DB = LDB.DB

-- | Types that can be serialized, stored and retrieved by Dash
-- A ProtoBuf with a key
class (ProtoBuf a) => Stashable a where
    key :: a -> Key

-- | Store the ProtoBuf in the database
--
stash :: Stashable s => DB -> s -> ResIO ()
stash db s = put db (key s) (toStrict $ encode s)

-- | Wrap and store the ProtoBuf in the database
--
stashWrapped :: Stashable s => DB -> s -> ResIO ()
stashWrapped db s = put db (key s) (toStrict $ encodeRaw $ wrap s)

-- | Fetch the ProtoBuf from the database
--
fetch :: (ProtoBuf a) => DB -> Key -> ResIO (Either ProtoFail a)
fetch db k = liftM decode_found $ get db k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decode $ toLazy bs

put :: DB -> Key -> ByteString -> ResIO ()
put db k = LDB.put db def $ packKey k

get :: DB -> Key -> ResIO (Maybe ByteString)
get db k = LDB.get db def $ packKey k

openDB :: FilePathS -> ResIO DB
openDB path =  LDB.open path
    LDB.defaultOptions{LDB.createIfMissing = True, LDB.cacheSize= 2048}
