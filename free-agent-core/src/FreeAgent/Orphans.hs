{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Toxic waste site for all the vile little instances
-- we have to define for types in other libraries
module FreeAgent.Orphans where

import           AgentPrelude

import           Control.Monad (mzero)
import           Data.Binary(Binary)
import qualified Data.Binary as Binary

import           Data.SafeCopy
import           Data.Aeson

import           System.Cron

import           Control.Monad.Logger          (MonadLogger(..))
import qualified Data.Serialize                as Cereal
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text.Encoding            as Text (decodeUtf8, encodeUtf8)
import           Data.Time                     (UTCTime(..),Day(..))

import Data.UUID (UUID)
import Data.Acid (AcidState)
import Data.Typeable (Typeable1)

import           Control.Distributed.Process (Closure)

deriving instance Typeable1 AcidState

--orphans from monad-logger
instance MonadLogger m => MonadLogger (EitherT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

-- orphans from convertible
instance Convertible ByteString String where
    safeConvert = return . BS.unpack

instance Convertible String ByteString where
    safeConvert = return . BS.pack

instance Convertible Text ByteString where
    safeConvert = return . Text.encodeUtf8

instance Convertible ByteString Text where
    safeConvert = return . Text.decodeUtf8

instance Convertible String FilePath where
    safeConvert = return . fpFromString

instance Convertible FilePath String where
    safeConvert = return . fpToString

instance Convertible Text FilePath where
    safeConvert = return . fpFromText

instance Convertible FilePath Text where
    safeConvert = return . fpToText

--orphans from Binary
instance Binary.Binary Text where
    put = Binary.put . Text.encodeUtf8
    get = Text.decodeUtf8 <$> Binary.get

instance Binary UTCTime where
 put (UTCTime (ModifiedJulianDay d) t) = do
        Binary.put d
        Binary.put (toRational t)
 get = do
        d <- Binary.get
        t <- Binary.get
        return $ UTCTime (ModifiedJulianDay d) (fromRational t)

-- orphans from cereal
instance Cereal.Serialize UTCTime where
 put (UTCTime (ModifiedJulianDay d) t) = do
        Cereal.put d
        Cereal.put (toRational t)
 get = do
        d <- Cereal.get
        t <- Cereal.get
        return $ UTCTime (ModifiedJulianDay d) (fromRational t)

instance Typeable a => Cereal.Serialize (Closure a) where
    get = safeGet
    put = safePut

-- ophans from safecopy
--
-- create safecopy instances for Binary types - this
-- is unsafe since migrations are impossible
class Binary a => UnsafeCopy a where
    unsafeGet :: Contained (Cereal.Get a)
    unsafePut :: a -> Contained Cereal.Put

    unsafeGet = contain $ Binary.decode <$> safeGet
    unsafePut = contain . safePut . Binary.encode

instance Typeable a => UnsafeCopy (Closure a)
instance Typeable a => SafeCopy (Closure a) where
    version = 1
    kind = base
    errorTypeName _ = "Control.Distributed.Static.Closure"
    putCopy = unsafePut
    getCopy = unsafeGet


deriveSafeStore ''UUID

-- orphans from filesystem-path
instance Binary FilePath where
    put = Binary.put . fpToString
    get = return . fpFromString =<< Binary.get

instance SafeCopy FilePath where
    version = 1
    kind = base
    errorTypeName _ = "Filesystem.Path.FilePath"
    putCopy = contain . safePut . fpToString
    getCopy = contain $ return . fpFromString =<< safeGet

instance FromJSON FilePath where
    parseJSON (Object value') = do
        path  <- value' .: "filepath"
        return $ fpFromText path
    parseJSON _ = mzero

instance ToJSON FilePath where
    toJSON path =
        object ["filepath" .= fpToText path]

-- orphans from cron
deriving instance Typeable CronSchedule


deriving instance Generic CronField
deriving instance Generic DayOfWeekSpec
deriving instance Generic DayOfMonthSpec
deriving instance Generic MinuteSpec
deriving instance Generic HourSpec
deriving instance Generic MonthSpec
deriving instance Generic CronSchedule

deriveSerializers ''CronField
deriveSerializers ''DayOfWeekSpec
deriveSerializers ''DayOfMonthSpec
deriveSerializers ''MinuteSpec
deriveSerializers ''HourSpec
deriveSerializers ''MonthSpec
deriveSerializers ''CronSchedule
