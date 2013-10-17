{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}

-- | Extensions to BasicPrelude that are useful to most modules & plugins in Dash
-- This module should NOT import from any Dash modules and anything
-- exported here should be used in at least two Dash modules
module Dash.Prelude
    (
      module BasicPrelude
    , module Control.Lens
    , showStr
    , FilePathS
    , debug, dbg
    , ConvertText(..)
    , def
    ) where

import           BasicPrelude

import qualified Prelude              as P

import           Debug.FileLocation   (debug, dbg)
import qualified Data.Text.Encoding   as Text (decodeUtf8, encodeUtf8)
import           Data.Text            (pack, unpack)

import           Data.Default         (def)

import           Control.Lens
    (makeFields, (.~), (^.), (&), view, Getting)



showStr :: (Show a) => a -> String
showStr = P.show

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
