{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ExistentialQuantification #-}
module Dash.Store(fetchProto, storeProto, get, put, openDB, DB, RunningStore(..)) where


import           BasicPrelude
import qualified Prelude as P
import           Control.Monad
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Resource      (release, ResIO)

import qualified Data.ByteString.Lazy              as LByteS
import           Data.Default                      (def)
import qualified Database.LevelDB                  as LDB
import           Text.ProtocolBuffers.Reflections
import           Text.ProtocolBuffers.WireMessage

import qualified Dash.Proto                        as Proto
import qualified Dash.Proto.Runnable.NagiosCommand as NC
import           Dash.Runner


type Key = ByteString
type DB = LDB.DB

-- | Types that can be serialized and stored by Dash
--
-- Dash.Store provides a key-value store for protobuf records; types implementing this class are compatible
-- One particular feature is that optional key prefixes are availble in order to support
-- efficient scans of partial keys; this allows for an ad-hoc hierarchy of up to three layers, for example:
-- ("accounts","domestic", "petroleum") - this would enable you to do efficient queries
-- to retrieve all accounts, all domestic accounts, all domestric petroleum accounts etc.
class (ReflectDescriptor a, Wire a, Runnable a, Show a, Eq a) => ProtoStore a where
    key       :: a -> Key
    keyPrefix :: a -> (Key, Key, Key)
    serialize     :: a -> ByteString
    deserialize   :: Maybe ByteString -> a

    keyPrefix _ = ("","","")
    serialize     ps = toStrict $ Proto.messagePut ps
    deserialize Nothing = error "Didn't find value for key"
    deserialize (Just bs) = case Proto.messageGet $ toLazy bs of
        Right (ps, remain) | LByteS.length remain == 0 ->
            ps
        Right (_, _) ->
            error "Failed to parse ProtoStore fully."
        Left error_message ->
            error $ "Failed to parse ProtoStore." ++ error_message

data RunningStore a = forall p. (ProtoStore p, Eq p) => RunningStore p

instance Eq (RunningStore a) where
    a == b = serialize a == serialize b

instance Show (RunningStore a) where
    show (RunningStore a) = "RunningStore (" ++ P.show a ++ ")"

instance ProtoStore NC.NagiosCommand where
    key cmd = toStrict $ Proto.utf8 $ NC.host cmd

instance ReflectDescriptor (RunningStore a) where
    getMessageInfo (RunningStore a) = getMessageInfo a
    reflectDescriptorInfo (RunningStore a) = reflectDescriptorInfo a

instance Wire (RunningStore a) where
    wireSize a (RunningStore s) = wireSize a s
    wirePut a (RunningStore s) = wirePut a s
    wireGet a = wireGet a

instance ProtoStore (RunningStore a) where
    key (RunningStore s) = key s
    keyPrefix (RunningStore s) = keyPrefix s
    serialize (RunningStore s) = serialize s
    deserialize bs = deserialize bs

instance Runnable (RunningStore a) where
    exec (RunningStore s) = exec s

storeProto :: ProtoStore ps => DB -> ps -> ResIO ()
storeProto db ps = put db (key ps) (serialize ps)

fetchProto :: (ProtoStore ps) => DB -> Key -> ResIO ps
fetchProto db k = liftM deserialize $ get db k

put :: DB -> Key -> ByteString -> ResIO ()
put db = LDB.put db def

get :: DB -> Key -> ResIO (Maybe ByteString)
get db = LDB.get db def

openDB :: P.FilePath -> ResIO DB
openDB path =  LDB.open path
    LDB.defaultOptions{LDB.createIfMissing = True, LDB.cacheSize= 2048}

toLazy :: ByteString -> LByteString
toLazy bs = LByteS.fromChunks [bs]

toStrict :: LByteString -> ByteString
toStrict = concat . LByteS.toChunks
