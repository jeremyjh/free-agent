{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}


-- | Extensions to ClassyPrelude that are useful to most modules & plugins in Dash
-- This module should NOT import from any Dash modules and anything
-- exported here should be used in at least two Dash modules
module FreeAgent.AgentPrelude
    (
      module ClassyPrelude
    , FilePathS
    , debug, dbg, err
    , convert
    , EitherT, runEitherT
    , convEither, convEitherT
    , tryAnyConvT
    , Convertible(..)
    , def
    , deriveSerializers
    , fqName, proxyFqName
    , qdebug, qinfo, qwarn, qerror
    , qdebugNS
    , logDebug, logInfo, logWarn, logError
    , Proxy(..)
    , deriveSafeStore
    , deriveSafeStoreVersion
    , getCurrentTime
    , NFData(..), genericRnf
    , (!??)
    , zeroDate
    ) where


import           ClassyPrelude                 hiding (handle, getCurrentTime, ask, ReaderT)
import qualified Prelude                       as P

import           Control.DeepSeq.Generics      (NFData(..), genericRnf)
import           Control.Error                 (EitherT, runEitherT, hoistEither)
import           Control.Monad.Logger          (logDebug, logInfo, logWarn, logError)
import           Control.Monad.Logger.Quote    (qdebug, qinfo, qwarn, qerror, qdebugNS)
import Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Binary                   as Binary (Binary (..))
import           Data.Convertible              (Convertible(..), convert)
import           Data.Default                  (def)
import           Data.Typeable
import           Language.Haskell.TH           (Dec, Name, Q)
import           Language.Haskell.TH.Lib       (conT)

import           Data.Aeson                    (FromJSON(..), ToJSON)
import qualified Data.Time.Clock as            Time
import           Data.SafeCopy
       (Version, deriveSafeCopy, base, extension)
import           FileLocation                  (dbg, debug, err)

#if __GLASGOW_HASKELL__ < 708
data Proxy a = Proxy deriving Typeable
#endif

type FilePathS = P.FilePath

fqName :: (Typeable a) => a -> Text
fqName typee =  modName ++ "." ++ name
  where
    name = pack . P.show $ typeOf typee
    modName = pack . tyConModule . typeRepTyCon $ typeOf typee

proxyFqName :: (Typeable a) => Proxy a -> Text
proxyFqName typee =  modName ++ "." ++ name
  where
    name = pack . P.show $ subtype
    subtype = let (_ , [t]) = (splitTyConApp $ typeOf typee) in t
    modName = pack . tyConModule $ typeRepTyCon subtype

convEither :: Convertible e f => Either e a -> Either f a
convEither (Right result) = Right result
convEither (Left reason) = Left $ convert reason

convEitherT :: (Convertible e f, Monad m)
            => Either e a -> EitherT f m a
convEitherT = hoistEither . convEither

-- | Template haskell function to create the Serialize and SafeCopy
-- instances for a given type - use this one to specify a later version
-- (also will require a migration instance - see SafeCopy docs for more info )
--
-- > data MyDataV1 = MyDataV1 Int
-- > data MyData = MyData Int String
-- > deriveSafeStore ''MyDataV1
-- > deriveSafeStoreVersion 2 ''MyData
deriveSafeStoreVersion :: Version a -> Name -> Q [Dec]
deriveSafeStoreVersion ver name = do
    sc <- case ver of
        1 -> deriveSafeCopy 1 'base name
        _ -> deriveSafeCopy ver 'extension name
    return $ sc

-- | Template haskell function to create the Serialize and SafeCopy
-- instances for a given type
--
-- > data MyData = MyData Int
-- > deriveSafeStore ''MyData
deriveSafeStore :: Name -> Q [Dec]
deriveSafeStore = deriveSafeCopy 1 'base

-- | Same as 'deriveSerializers' except that the 'SafeCopy' instance will be
-- for an extension of the provided version. This would also require
-- a migration from the previous verison. See the SafeCopy documentation
-- for more details.
deriveSerializersVersion :: Version a -> Name -> Q [Dec]
deriveSerializersVersion ver name = do
    sc <- deriveSafeStoreVersion ver name
    gen <- [d| instance Binary   $(conT name)
               instance FromJSON $(conT name)
               instance ToJSON   $(conT name)
               instance NFData   $(conT name)
                   where rnf = genericRnf |]
    return $ sc ++ gen

-- | TemplateHaskell function to generate required serializers and related
-- instances for Actions/Results.
-- This includes Cereal, SafeCopy, Binary and NFData.
deriveSerializers :: Name -> Q [Dec]
deriveSerializers = deriveSerializersVersion 1


(!??) :: Applicative m => m (Maybe a) -> e -> m (Either e a)
(!??) ma e = toeither <$> ma
  where toeither Nothing = Left e
        toeither (Just a) = Right a

tryAnyConvT :: (MonadBaseControl IO io, Convertible SomeException e)
            => io a -> EitherT e io a
tryAnyConvT ma = lift (tryAny ma) >>= convEitherT


getCurrentTime :: MonadIO io => io UTCTime
getCurrentTime = liftIO Time.getCurrentTime

zeroDate ::UTCTime
zeroDate = convert (0::Int)
