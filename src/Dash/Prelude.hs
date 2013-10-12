{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}

module Dash.Prelude
    (
      module BasicPrelude
    , module Control.Lens
    , showStr
    , toLazy, toStrict
    , split
    , FilePathS
    , debug
    , dbg
    , ConvertText(..)
    , ask, asks
    ) where

import           BasicPrelude

import qualified Data.ByteString.Lazy as LByteS
import qualified Prelude              as P

import           Data.String.Utils    (split)
import           Debug.FileLocation
import qualified Data.Text.Encoding   as Text (decodeUtf8, encodeUtf8)
import           Data.Text            (pack, unpack)
import           Control.Monad.Reader (ask, asks)

import           Control.Lens         (makeFields, (.~), (^.), (&))



showStr :: (Show a) => a -> String
showStr = P.show

toLazy :: ByteString -> LByteString
toLazy bs = LByteS.fromChunks [bs]

toStrict :: LByteString -> ByteString
toStrict = concat . LByteS.toChunks

type FilePathS = P.FilePath

class ConvertText a where
    toT :: a -> Text
    fromT :: Text -> a

instance ConvertText ByteString where
    toT = Text.decodeUtf8
    fromT = Text.encodeUtf8

instance ConvertText String where
    toT = pack
    fromT = unpack
