{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Dash.Prelude
    (
      module BasicPrelude
    , tee
    , showStr
    , toLazy, toStrict
    , split
    , FilePathS
    ) where

import           BasicPrelude

import qualified Data.ByteString.Lazy as LByteS
import qualified Prelude              as P
import           Debug.Trace          (traceShow)

import           Data.String.Utils    (split)



tee :: (Show a) => a -> a
tee = join traceShow

showStr :: (Show a) => a -> String
showStr = P.show

toLazy :: ByteString -> LByteString
toLazy bs = LByteS.fromChunks [bs]

toStrict :: LByteString -> ByteString
toStrict = concat . LByteS.toChunks

type FilePathS = P.FilePath
