{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Extensions to ClassyPrelude that are useful to most modules & plugins in Dash
-- This module should NOT import from any Dash modules and anything
-- exported here should be used in at least two Dash modules
module AgentPrelude
    (
      module ClassyPrelude
    , showStr
    , FilePathS
    , debug, dbg, err
    , convEither, convEitherT
    , Convertible(..)
    , ConvertText(..)
    , ConvertByteString(..)
    , def
    , P.undefined --classy undefined is obnoxious
    , Generic
    , deriveSerializers
    , fqName
    , qdebug, qinfo, qwarn, qerror
    , qdebugNS
    , logDebug, logInfo, logWarn, logError
    , forceMaybeMsg
    , forceEither, forceEitherT
    , deriveStorable
    , deriveStorableVersion
    ) where

import           ClassyPrelude                 hiding (undefined)
import qualified Prelude                       as P

import           Control.DeepSeq.TH            (deriveNFData)
import           Control.Error                 (runEitherT, EitherT, hoistEither)
import           Control.Monad.Logger          (MonadLogger(..), logDebug, logInfo, logWarn, logError)
import           Control.Monad.Logger.Quote    (qdebug, qinfo, qwarn, qerror, qdebugNS)
import           Data.Binary                   as Binary (Binary (..))
import qualified Data.Binary                   as Binary
import qualified Data.Serialize                as Cereal
import qualified Data.ByteString.Char8         as BS
import           Data.Default                  (def)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text (decodeUtf8, encodeUtf8)
import           Data.Time                     (UTCTime(..),Day(..))
import           Data.Typeable
import           GHC.Generics                  (Generic)
import           Language.Haskell.TH           (Dec, Name, Q)
import           Language.Haskell.TH.Lib       (conT)

import           Data.Maybe.Utils (forceMaybeMsg)
import           Data.Either.Utils (forceEither)
import           Data.SafeCopy
       (Version, deriveSafeCopy, base, extension, safeGet, safePut)
import           Data.UUID                     (toASCIIBytes, fromASCIIBytes, UUID)
import           FileLocation                  (dbg, debug, err)

class Convertible a b where
    convert :: a -> b

instance MonadLogger m => MonadLogger (EitherT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

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

instance Binary UTCTime where
 put (UTCTime (ModifiedJulianDay d) t) = do
        put d
        put (toRational t)
 get = do
        d <- get
        t <- get
        return $ UTCTime (ModifiedJulianDay d) (fromRational t)

instance Cereal.Serialize UTCTime where
 put (UTCTime (ModifiedJulianDay d) t) = do
        Cereal.put d
        Cereal.put (toRational t)
 get = do
        d <- Cereal.get
        t <- Cereal.get
        return $ UTCTime (ModifiedJulianDay d) (fromRational t)

instance ConvertByteString UTCTime where
    toBytes = Cereal.encode
    fromBytes bs = let (Right time) = Cereal.decode bs in time

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

forceEitherT :: (Show e, Monad m) => EitherT e m a -> m a
forceEitherT ema = runEitherT ema >>= return . forceEither

convEither :: Convertible e f => Either e a -> Either f a
convEither (Right result) = Right result
convEither (Left reason) = Left $ convert reason

convEitherT :: (Convertible e f, Monad m)
            => Either e a -> EitherT f m a
convEitherT = hoistEither . convEither

-- | Template haskell function to create the Serialize and SafeCopy
-- instances for a given type
--
-- > data MyData = MyData Int
-- > deriveStorable ''MyData
deriveStorable :: Name -> Q [Dec]
deriveStorable = deriveStorableVersion 1

-- | Template haskell function to create the Serialize and SafeCopy
-- instances for a given type - use this one to specify a later version
-- (also will require a migration instance - see SafeCopy docs for more info )
--
-- > data MyDataV1 = MyDataV1 Int
-- > data MyData = MyData Int String
-- > deriveStorable ''MyDataV1
-- > deriveStorableVersion 2 ''MyData
deriveStorableVersion :: Version a -> Name -> Q [Dec]
deriveStorableVersion ver name = do
    sc <- case ver of
        1 -> deriveSafeCopy 1 'base name
        _ -> deriveSafeCopy ver 'extension name
    ss <- [d| instance Cereal.Serialize $(conT name) where
                get = safeGet
                put = safePut
          |]
    return $ sc ++ ss
