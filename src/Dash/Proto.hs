{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- for IsString Utf8

module Dash.Proto
    (
      Utf8, ConvertUtf8(..)
    , defaultValue
    , ProtoBuf(..)
    , messageGet, messagePut
    , ReflectDescriptor(..), Wire(..)
    , Wrapper(..)
    , wrap
    , unWrap
    , decodeRaw
    , encodeRaw
    , ProtoFail(..)
    ) where

import           Dash.Prelude
import qualified Data.ByteString.Lazy              as LByteS
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Text.ProtocolBuffers.Basic(uToString,uFromString,Utf8,utf8, defaultValue)
import           Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire(..))
import           Text.ProtocolBuffers.Reflections (ReflectDescriptor(..), descName, ProtoName(..))
import           Text.ProtocolBuffers.Identifiers (FIName(..))

import           Dash.Proto.Common.Wrapper

instance IsString Utf8 where fromString = uFromString

-- | A protobuf inside a Wrapper for direct & polymorphic decode (ala Dash.Action)
--
class (ReflectDescriptor a, Wire a, Typeable a, Eq a, Show a) => ProtoBuf a where
    encode :: a -> LByteString
    decode :: LByteString -> Either ProtoFail a

    encode = encodeRaw
    decode = decodeRaw

data ProtoFail = BytesLeftover | ParseFail String | NotFound String deriving (Show, Eq)

instance ProtoBuf Wrapper

encodeRaw :: (ReflectDescriptor a, Wire a) => a -> LByteString
encodeRaw = messagePut

decodeRaw :: (ReflectDescriptor a, Wire a) => LByteString -> Either ProtoFail a
decodeRaw bs = case messageGet bs of
    Right (ps, remain) | LByteS.length remain == 0 ->
        return ps
    Right (_, _) -> Left BytesLeftover
    Left error_message ->
        Left $ ParseFail error_message

wrap :: (ProtoBuf a) => a -> Wrapper
wrap pb = Wrapper {typeName = typeNameOf pb, value = encodeRaw pb}
  where
    typeNameOf = fiName . protobufName . descName . reflectDescriptorInfo

unWrap :: (ProtoBuf a) => Wrapper -> Either ProtoFail a
unWrap = decodeRaw . value

-- | Make dealing with the ProtoBuf package's Utf8 type less terrible
--
class ConvertUtf8 a where
    toU :: a -> Utf8
    fromU :: Utf8 -> a

instance ConvertUtf8 String where
    toU = uFromString
    fromU = uToString

instance ConvertUtf8 ByteString where
    toU = undefined
    fromU = toStrict . utf8
