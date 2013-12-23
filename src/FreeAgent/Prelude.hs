{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric        #-}

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
    , def
    , P.undefined --classy undefined is obnoxious
    , Generic
    , deriveSerializers
    , fqName
    , logM
    , utcToBytes
    , bytesToUtc
    , runAgentLoggingT
    , logDebug
    , logWarn
    ) where

import           ClassyPrelude hiding    (undefined)
import qualified Prelude                 as P
import           Debug.FileLocation      (debug, dbg)

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Logger
    ( LoggingT, runStdoutLoggingT, withChannelLogger
    , logDebug, logWarn )
import qualified Data.Text.Encoding      as Text (decodeUtf8, encodeUtf8)
import qualified Data.Text               as Text
import           Data.Default            (def)
import           Data.Binary             as Binary (Binary(..))
import qualified Data.ByteString.Char8   as BS
import           Data.Typeable


import           Database.LevelDB.Higher.Store (deriveStorableVersion, Version)
import           GHC.Generics            (Generic)
import           Language.Haskell.TH (Name, Q, Dec)
import           Language.Haskell.TH.Lib (conT)


import           System.Locale           (defaultTimeLocale)
import           Data.Time               (UTCTime)
import           Data.Time.Format        (formatTime, parseTime)

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

deriveSerializers :: Name -> Q [Dec]
deriveSerializers = deriveSerializersVersion 1

deriveSerializersVersion :: Version a -> Name -> Q [Dec]
deriveSerializersVersion ver name = do
    sc <- deriveStorableVersion ver name
    bi <- [d| instance Binary $(conT name) where |]
    return $ sc ++ bi

fqName :: (Typeable a) => a -> ByteString
fqName typee =  modName ++ "." ++ name
  where
    name = BS.pack $ P.show $ typeOf typee
    modName = BS.pack $ tyConModule $ typeRepTyCon $ typeOf typee

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

runAgentLoggingT :: (MonadIO m, MonadBaseControl IO m) => LoggingT m a -> m a
runAgentLoggingT = runStdoutLoggingT . withChannelLogger 50
