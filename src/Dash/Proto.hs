{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- for IsString Utf8

module Dash.Proto
    ( uToString
    , uFromString
    , Utf8
    , utf8
    , defaultValue
    , messageGet
    , messagePut
    , ProtoBuf(..)
    , toStrict
    ) where

import           BasicPrelude
import qualified Data.ByteString.Lazy              as LByteS
import           Text.ProtocolBuffers.Basic(uToString,uFromString,Utf8,utf8, defaultValue)
import           Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire(..))
import           Text.ProtocolBuffers.Reflections (ReflectDescriptor(..))


instance IsString Utf8 where fromString = uFromString

class (ReflectDescriptor a, Wire a, Eq a, Show a) => ProtoBuf a where
    encode :: a -> ByteString
    decode :: ByteString -> a

    encode ps = toStrict $ messagePut ps
    decode bs = case messageGet $ toLazy bs of
        Right (ps, remain) | LByteS.length remain == 0 ->
            ps
        Right (_, _) ->
            error "Failed to parse ProtoStore fully."
        Left error_message ->
            error $ "Failed to parse ProtoStore." ++ error_message

toLazy :: ByteString -> LByteString
toLazy bs = LByteS.fromChunks [bs]

toStrict :: LByteString -> ByteString
toStrict = concat . LByteS.toChunks
