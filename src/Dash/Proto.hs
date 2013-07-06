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
    ) where

import           BasicPrelude
import           Text.ProtocolBuffers.Basic(uToString,uFromString,Utf8,utf8, defaultValue)
import           Text.ProtocolBuffers.WireMessage (messageGet, messagePut)

instance IsString Utf8 where fromString = uFromString
