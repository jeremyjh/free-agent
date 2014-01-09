{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Extensions to ClassyPrelude that are useful to most modules & plugins in Dash
-- This module should NOT import from any Dash modules and anything
-- exported here should be used in at least two Dash modules
module FreeAgent.Prelude
    (
      module ClassyPrelude
    , showStr
    , FilePathS
    , debug, dbg
    , ConvertText(..)
    , ConvertByteString(..)
    , def
    , P.undefined --classy undefined is obnoxious
    , Generic
    , deriveSerializers
    , fqName
    , logM
    , utcToBytes
    , bytesToUtc
    , logDebug
    , Log.logWarn
    ) where

import           ClassyPrelude                 hiding (undefined)
import qualified Prelude                       as P

import           Control.DeepSeq.TH            (deriveNFData)
import qualified Control.Monad.Logger          as Log (logDebug, logWarn)
import           Data.Binary                   as Binary (Binary (..))
import qualified Data.ByteString.Char8         as BS
import           Data.Default                  (def)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text (decodeUtf8, encodeUtf8)
import           Data.Time                     (UTCTime)
import           Data.Time.Format              (formatTime, parseTime)
import           Data.Typeable
import           GHC.Generics                  (Generic)
import           Language.Haskell.TH           (Dec, Exp, Name, Q)
import           Language.Haskell.TH.Lib       (conT)
import           System.Locale                 (defaultTimeLocale)

import           Database.LevelDB.Higher.Store (Version, deriveStorableVersion)
import           Data.UUID                     (toASCIIBytes, fromASCIIBytes, UUID)
import           Debug.FileLocation            (dbg, debug)

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

instance Binary.Binary Text where
    put = Binary.put . Text.encodeUtf8
    get = Text.decodeUtf8 <$> Binary.get

class ConvertByteString a where
    toBytes :: a -> ByteString
    fromBytes :: ByteString -> a

instance ConvertByteString UUID where
    toBytes = toASCIIBytes
    fromBytes uuid = fromMaybe  (error "invalid UUUID Bytes") (fromASCIIBytes uuid)

-- | TemplateHaskell function to generate required serializers and related
-- instances for Actions/Results.
-- This includes Cereal, SafeCopy, Binary and NFData.
deriveSerializers :: Name -> Q [Dec]
deriveSerializers = deriveSerializersVersion 1

-- | Same as 'deriveSerializers' except that the 'SafeCopy' instance will be
-- for an extension of the provided version. This would also require
-- a migration from the previous verison. See the SafeCopy documentation
-- for more details.
deriveSerializersVersion :: Version a -> Name -> Q [Dec]
deriveSerializersVersion ver name = do
    sc <- deriveStorableVersion ver name
    bi <- [d| instance Binary $(conT name) where |]
    nf <- deriveNFData name
    return $ sc ++ bi ++ nf

fqName :: (Typeable a) => a -> ByteString
fqName typee =  modName ++ "." ++ name
  where
    name = BS.pack . P.show $ typeOf typee
    modName = BS.pack . tyConModule . typeRepTyCon $ typeOf typee

-- TODO: use real logging
logM :: (MonadIO m) => Text -> m()
logM = putStrLn

-- | Convert UTCTime to ByteString - the format of the
-- ByteString is epoch seconds append with (12 digit padded) pico seconds.
utcToBytes :: UTCTime -> ByteString
utcToBytes = BS.pack . formatTime defaultTimeLocale "%s%q"

-- | Convert ByteString timestamp to UTCTime
-- throws an Error if parsing fails
bytesToUtc :: ByteString -> UTCTime
bytesToUtc bs =
    case parseTime defaultTimeLocale "%s%q" $ BS.unpack bs of
        Nothing -> error $
            "Failed to parse UTCTime: " ++ (BS.unpack bs)
        Just t -> t

logDebug :: Q Exp
logDebug =
--TODO: benchmark this with large amount of data once we
--have lots of debug statements in code
#ifdef NO_DEBUG
  [e|return . const ()|]
#else
  Log.logDebug
#endif
