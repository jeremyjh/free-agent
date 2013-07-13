{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.Store(fetchProto, readCommand, storeProto, writeCommand, getV, putV, openDB) where


import           BasicPrelude
import qualified Prelude as P
import           Control.Monad
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Resource      (release, ResIO)

import qualified Data.ByteString.Lazy              as LByteS

import           Data.Default                      (def)
import           Database.LevelDB

import qualified Dash.Proto                        as Proto
import qualified Dash.Proto.Runnable.NagiosCommand as NC
import           Text.ProtocolBuffers.Reflections  (ReflectDescriptor)
import           Text.ProtocolBuffers.WireMessage  (Wire)

-- | Types that can be serialized and stored by Dash
--
-- Dash.Store provides a key-value store for protobuf records; types implementing this class are compatible
-- One particular feature is that optional key prefixes are availble in order to support
-- efficient scans of partial keys; this allows for an ad-hoc hierarchy of up to three layers, for example:
-- ("accounts","domestic", "petroleum") - this would enable you to do efficient queries
-- to retrieve all accounts, all domestic accounts, all domestric petroleum accounts etc.
class (ReflectDescriptor a, Wire a) => ProtoStore a where
    key       :: a -> ByteString
    keyPrefix :: a -> (ByteString, ByteString, ByteString)
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

instance ProtoStore NC.NagiosCommand where
    key cmd = toStrict $ Proto.utf8 $ NC.host cmd

storeProto :: ProtoStore ps => DB -> ps -> ResIO ()
storeProto db ps = put db def (key ps) (serialize ps)

fetchProto :: ProtoStore ps => DB -> ByteString -> ResIO ps
fetchProto db key = liftM deserialize $ get db def key

readCommand :: DB -> ByteString -> ResIO NC.NagiosCommand
readCommand db key =
    get db def key >>= parseCmd
  where
    parseCmd Nothing = error "Didn't find value for key"
    parseCmd (Just input) = case Proto.messageGet $ toLazy input of
        Right (cmd, remain) | LByteS.length remain == 0 ->
            return cmd
        Right (_, _) ->
            error "Failed to parse Command fully."
        Left error_message ->
            error $ "Failed to parse Command." ++ error_message


writeCommand :: DB -> NC.NagiosCommand -> ResIO ()
writeCommand db cmd = do
    put db def
        (toStrict $ Proto.utf8 $ NC.host cmd)
        (toStrict $ Proto.messagePut cmd)
    return ()

putV :: DB -> ByteString -> ByteString -> ResIO ()
putV db key value = put db def key value

getV :: DB -> ByteString -> ResIO (Maybe ByteString)
getV db key = get db def key

openDB :: P.FilePath -> ResIO DB
openDB path =  open path
    defaultOptions{createIfMissing = True, cacheSize= 2048}

toLazy :: ByteString -> LByteString
toLazy bs = LByteS.fromChunks [bs]

toStrict :: LByteString -> ByteString
toStrict = concat . LByteS.toChunks
