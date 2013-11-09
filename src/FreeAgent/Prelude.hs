{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}

-- | Extensions to BasicPrelude that are useful to most modules & plugins in Dash
-- This module should NOT import from any Dash modules and anything
-- exported here should be used in at least two Dash modules
module FreeAgent.Prelude
    (
      module ClassyPrelude
    , module Control.Lens
    , showStr
    , FilePathS
    , debug, dbg
    , ConvertText(..)
    , def
    , P.undefined --classy undefined is obnoxious
    ) where

{-import           BasicPrelude         hiding (print)-}
import           ClassyPrelude hiding (undefined)

import qualified Prelude              as P

import           Debug.FileLocation   (debug, dbg)
import qualified Data.Text.Encoding   as Text (decodeUtf8, encodeUtf8)
import           Data.Text            as Text

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
    toT = Text.pack
    fromT = Text.unpack

instance ConvertText FilePath where
    toT = fpToText
    fromT = fpFromText
