{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Extensions to ClassyPrelude that are useful to most modules & plugins in Dash
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
    , deriveBinary
    , fqName
    ) where

import           ClassyPrelude hiding    (undefined)

import qualified Prelude                 as P

import           Debug.FileLocation      (debug, dbg)
import qualified Data.Text.Encoding      as Text (decodeUtf8, encodeUtf8)
import qualified Data.Text               as Text

import           Data.Default            (def)
import           Data.Binary             (Binary(..))
import qualified Data.ByteString.Char8   as BS
import           Data.Typeable

import           Control.Lens
    ((.~), (^.), (&), view, Getting)
import           Data.DeriveTH

import           Language.Haskell.TH.All (Name, Q, Dec)

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

instance Binary Text where
    put = put . Text.encodeUtf8
    get = Text.decodeUtf8 <$> get

deriveBinary :: Name -> Q [Dec]
deriveBinary t = derive makeBinary t

fqName :: (Typeable a) => a -> ByteString
fqName typee =  modName ++ "." ++ name
  where
    name = BS.pack $ P.show $ typeOf typee
    modName = BS.pack $ tyConModule $ typeRepTyCon $ typeOf typee
