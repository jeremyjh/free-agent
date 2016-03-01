{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell                         #-}


-- | Extensions to ClassyPrelude that are useful to most modules & plugins in Dash
-- This module should NOT import from any Dash modules and anything
-- exported here should be used in at least two Dash modules
module FreeAgent.AgentPrelude
    (
      module BasicPrelude
    , Semigroup(..)
    , forM_, forM
    , Generic
    , Path.FilePath
    , (</>)
    , P.show
    , tshow
    , Time.UTCTime(..)
    , Time.Day(..)
    , FilePathS
    , debug, dbg, err
    , convert
    , ExceptT, runExceptT
    , convEither, convExceptT
    , tryAny
    , catchAny
    , tryAnyConvT
    , Convertible(..)
    , def
    , deriveSerializers
    , fqName, typeName, proxyFqName
    , qdebug, qinfo, qwarn, qerror
    , qdebugNS
    , logDebug, logInfo, logWarn, logError
    , Proxy(..)
    , deriveSafeStore
    , deriveSafeStoreVersion
    , deriveSerializersVersion
    , getCurrentTime
    , NFData(..), genericRnf
    , (!??)
    , zeroDate
    ) where


import           BasicPrelude                    hiding (FilePath, forM, forM_, handle,
                                                  init, show, (</>), (<>))
import qualified Prelude                         as P

import           Control.DeepSeq.Generics        (NFData (..), genericRnf)
import           Control.Error                   (ExceptT, hoistEither, runExceptT)
import           Control.Monad.Logger            (logDebug, logError, logInfo, logWarn)
import           Control.Monad.Logger.Quote      (qdebug, qdebugNS, qerror, qinfo, qwarn)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Data.Binary                     as Binary (Binary (..))
import           Data.Convertible                (Convertible (..), convert)
import           Data.Convertible.Instances.Text ()
import           Data.Default                    (def)
import           Data.Typeable
import           GHC.Generics                    (Generic)
import           Language.Haskell.TH             (Dec, Name, Q)
import           Language.Haskell.TH.Lib         (conT)

import           Data.Aeson                      (FromJSON (..), ToJSON)
import           Data.Foldable                   (forM_)
import           Data.SafeCopy                   (Version, base, deriveSafeCopy,
                                                  extension)
import           Data.Semigroup                  (Semigroup (..))
import qualified Data.Text                       as Text
import qualified Data.Time.Calendar              as Time
import qualified Data.Time.Clock                 as Time
import           Data.Traversable                (forM)
import           FileLocation                    (dbg, debug, err)
import           Filesystem.Path                 ((</>))
import qualified Filesystem.Path.CurrentOS       as Path

import           Control.Exception.Enclosed      (catchAny, tryAny)

#if __GLASGOW_HASKELL__ < 708
data Proxy a = Proxy deriving Typeable
#endif

type FilePathS = P.FilePath

tshow :: (Show a) => a -> Text
tshow = Text.pack . P.show

typeName :: (Typeable a) => a -> Text
typeName typee = Text.pack . P.show $ typeOf typee

fqName :: (Typeable a) => a -> Text
fqName typee =  modName ++ "." ++ typeName typee
  where
    modName = Text.pack . tyConModule . typeRepTyCon $ typeOf typee

proxyFqName :: (Typeable a) => Proxy a -> Text
proxyFqName typee =  modName ++ "." ++ name
  where
    name = Text.pack . P.show $ subtype
    subtype = let (_ , [t]) = (splitTyConApp $ typeOf typee) in t
    modName = Text.pack . tyConModule $ typeRepTyCon subtype

convEither :: Convertible e f => Either e a -> Either f a
convEither (Right result) = Right result
convEither (Left reason) = Left $ convert reason

convExceptT :: (Convertible e f, Monad m)
            => Either e a -> ExceptT f m a
convExceptT = hoistEither . convEither

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
    sc <- case ver of
        1 -> deriveSafeCopy 1 'base name
        _ -> deriveSafeCopy ver 'extension name
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
deriveSerializers name =
    -- Originally we called deriveSerializersVersion here but this created
    -- random linking errors in hdevtools and other tools due (I suspect)
    -- to stage restriction. A little redundancy is preferable to separate
    -- modules.
 do sc <- deriveSafeCopy 1 'base name
    gen <- [d| instance Binary   $(conT name)
               instance FromJSON $(conT name)
               instance ToJSON   $(conT name)
               instance NFData   $(conT name)
                   where rnf = genericRnf |]
    return $ sc ++ gen


(!??) :: Applicative m => m (Maybe a) -> e -> m (Either e a)
(!??) ma e = toeither <$> ma
  where toeither Nothing = Left e
        toeither (Just a) = Right a

tryAnyConvT :: (MonadBaseControl IO io, Convertible SomeException e)
            => io a -> ExceptT e io a
tryAnyConvT ma = lift (tryAny ma) >>= convExceptT

getCurrentTime :: MonadIO io => io Time.UTCTime
getCurrentTime = liftIO Time.getCurrentTime

zeroDate :: Time.UTCTime
zeroDate = convert (0::Int)
