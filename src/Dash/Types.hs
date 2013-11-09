{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}


module Dash.Types
 ( module Dash.Types
 , Storeable, MonadLevelDB, Key, Value
 , MonadProcess
 )

where

import           Dash.Prelude
import qualified Prelude              as P
import           Control.Monad.Reader (ReaderT, ask)

import           Data.Typeable        (mkTyConApp, mkTyCon3)
import           Data.SafeCopy        (deriveSafeCopy, base, safeGet, safePut)
import           Data.Serialize       (Serialize(..))
import           Data.Default         (Default(..))
import           Data.Dynamic         (Dynamic)

import           Control.Monad.Writer (Writer)

import          Database.LevelDB.Higher
    (LevelDBT, MonadLevelDB,Storeable, Key, Value, FetchFail)

import          Control.Distributed.Process.Lifted
import          Control.Monad.Base (MonadBase)
import          Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO, MonadResource)
import          Control.Monad.Trans.Control





-- | Wrapper lets us store an Actionnd recover it using
-- the type name in 'registerUnWrappers'
data Wrapped = Wrapped { _wrappedTypeName :: ByteString
                       , _wrappedValue :: ByteString }
                deriving (Show, Typeable)

deriveSafeCopy 1 'base ''Wrapped

instance Serialize Wrapped where
    get = safeGet
    put = safePut

-- | Types that can be serialized, stored and retrieved
--
class (Storeable a) => Stashable a where
    key :: a -> Key

data Action = forall p. (Stashable p, Runnable p, Typeable p) => Action p

instance Typeable Action where
    typeOf _ = mkTyConApp (mkTyCon3 "dash" "Dash.Action" "Action") []

instance P.Show Action where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance Runnable Action where
    exec (Action a) = exec a

type FetchAction = Either FetchFail Action

type UnWrapper a = (Wrapped -> Either FetchFail a)
type Plugins = Map ByteString (UnWrapper Action)
type PluginConfigs = Map Text Dynamic
type PluginWriter = Writer [(ByteString, UnWrapper Action)] ()

data AgentConfig = AgentConfig { _configPlugins :: Plugins
                               , _configPluginConfigs :: PluginConfigs
                               , _configDbPath :: FilePathS
                               , _configNodeHost :: String
                               , _configNodePort :: String
                               }

instance Default AgentConfig where
    def = AgentConfig mempty mempty "./db" "127.0.0.1" "3546"

class (Monad m) => ConfigReader m where
    askConfig :: m AgentConfig

instance (Monad m) => ConfigReader (ReaderT AgentConfig m) where
    askConfig = ask

type MonadAgent m = (ConfigReader m, MonadProcess m, MonadLevelDB m)

newtype Agent a = Agent { unAgent :: ReaderT AgentConfig (LevelDBT Process) a}
            deriving ( Functor, Applicative, Monad, MonadBase IO
                     , ConfigReader, MonadIO, MonadThrow, MonadUnsafeIO
                     , MonadResource, MonadLevelDB
                     )
instance MonadBaseControl IO Agent where
  newtype StM Agent a = StAgent {unSTAgent :: StM (ReaderT AgentConfig (LevelDBT Process)) a}
  restoreM (StAgent m) = Agent $ restoreM m
  liftBaseWith f = Agent $ liftBaseWith $ \ rib -> f (fmap StAgent . rib . unAgent)

instance MonadProcess Agent where
    liftProcess ma = Agent $ lift $ lift ma

data RunStatus a = Running ProcessId
                   | Complete a
                   | Failed Text
    deriving (Show, Eq)

class Runnable a where
    exec :: a -> Agent (RunStatus Dynamic)
