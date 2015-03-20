{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell                            #-}

{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Toxic waste site for all the vile little instances
-- we have to define for types in other libraries
module FreeAgent.Orphans where

import           FreeAgent.AgentPrelude

import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary

import           Data.Aeson                  as Aeson
import           Data.HashMap.Strict         as H
import           Data.SafeCopy
import           Data.Scientific             (Scientific)

import           System.Cron

import           Control.Monad.Logger        (MonadLogger (..))
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import qualified Data.Serialize              as Cereal
import qualified Data.Text.Encoding          as Text (decodeUtf8, encodeUtf8)
import qualified Data.Text                   as Text

import qualified Data.CircularList as CL
import Data.CircularList (CList)

import           Data.Acid                   (AcidState)
import Data.Convertible (ConvertError(..))
import           Data.UUID                   (UUID)
import qualified Filesystem.Path.CurrentOS as F

import           Control.Distributed.Process (Closure)

#if __GLASGOW_HASKELL__ < 708
import           Data.Typeable               (Typeable1)
deriving instance Typeable1 AcidState
#else
deriving instance Typeable AcidState
#endif

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

instance Convertible LByteString String where
    safeConvert = return . LBS.unpack

instance Convertible String LByteString where
    safeConvert = return . LBS.pack

instance Convertible String FilePath where
    safeConvert = return . F.decodeString

instance Convertible FilePath String where
    safeConvert = return . F.encodeString

instance Convertible Text FilePath where
    safeConvert = return . F.fromText

instance Convertible FilePath Text where
    safeConvert fp =
        case F.toText fp of
            Right t -> return t
            Left reason -> Left (ConvertError (F.encodeString fp)
                                              "FilePath"
                                              "Text"
                                              (Text.unpack reason))

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

instance Show a => Convertible a Text where
    safeConvert = return . tshow


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
    put = Binary.put . F.encodeString
    get = return . F.decodeString =<< Binary.get

instance SafeCopy FilePath where
    version = 1
    kind = base
    errorTypeName _ = "Filesystem.Path.FilePath"
    putCopy = contain . safePut . F.encodeString
    getCopy = contain $ return . F.decodeString =<< safeGet

instance FromJSON FilePath where
    parseJSON (Object value') = do
        path  <- value' .: "filepath"
        return $ F.fromText path
    parseJSON _ = mzero

instance ToJSON FilePath where
    toJSON path =
        object ["filepath" .= F.toText path]

deriveSafeStore ''Value
deriveSafeStore ''Scientific

-- | An instance of SafeCopy for the Object Value.
instance (SafeCopy a, Eq a, Hashable a, SafeCopy b) => SafeCopy (H.HashMap a b) where
    getCopy = contain $ fmap H.fromList safeGet
    putCopy = contain . safePut . H.toList

instance (Binary a, Binary b, Hashable a, Eq a) => Binary (H.HashMap a b) where
    get = H.fromList <$> Binary.get
    put = Binary.put . H.toList

instance Binary.Binary Aeson.Value where
    put = Binary.put . Aeson.encode
    get =
     do bs <- Binary.get
        case Aeson.decode bs of
            Just v -> return v
            Nothing -> error $ "Could not decode to Aeson.Value a BytesString: " ++ convert bs

instance (SafeCopy a) => SafeCopy (CList a) where
    getCopy = contain $ fmap CL.fromList safeGet
    putCopy = contain . safePut . CL.rightElements

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
