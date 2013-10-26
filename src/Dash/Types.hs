{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Dash.Types where

import           Dash.Prelude
import qualified Prelude              as P
import           Control.Monad.Reader (ReaderT, ask)

import           Data.Typeable        (mkTyConApp, mkTyCon3)
import           Data.SafeCopy        (deriveSafeCopy, base, safeGet, safePut)
import           Data.Serialize       (Serialize(..))
import           Data.Default         (Default(..))

import           Dash.Store           (Stashable(..), FetchFail)
import           Control.Monad.Writer (Writer)

import          Database.LevelDB.Higher (LevelDBT, MonadLevelDB)
import          Control.Distributed.Process (Process)
import          Control.Distributed.Process.Lifted (MonadProcess(..))
import          Control.Monad.Base (MonadBase)
import          Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO, MonadResource)



data RunStatus = Running (Maybe String)
               | Complete (Maybe String)
               | Failed (Maybe String)
    deriving (Show, Eq)

class Runnable a where
    exec :: a -> IO RunStatus


-- | Wrapper lets us store an Actionnd recover it using
-- the type name in 'registerUnWrappers'
data Wrapped = Wrapped { typeName :: ByteString
                       , value :: ByteString }
                deriving (Show, Typeable)

deriveSafeCopy 1 'base ''Wrapped

instance Serialize Wrapped where
    get = safeGet
    put = safePut

data Action = forall p. (Stashable p, Runnable p, Typeable p) => Action p

instance Typeable Action where
    typeOf _ = mkTyConApp (mkTyCon3 "dash" "Dash.Action" "Action") []

instance P.Show Action where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance Runnable Action where
    exec (Action a) = exec a

type FetchAction = Either FetchFail Action

type UnWrapper a = (Wrapped -> Either FetchFail a)
type PluginMap = Map ByteString (UnWrapper Action)
type PluginWriter = Writer [(ByteString, UnWrapper Action)] ()

data AgentConfig = AgentConfig { _configPlugins :: PluginMap
                               , _configDbPath :: FilePathS
                               , _configNodeHost :: String
                               , _configNodePort :: String
                               }

instance Default AgentConfig where
    def = AgentConfig mempty "./db" "127.0.0.1" "3546"

makeFields ''AgentConfig

class (Monad m) => ConfigReader m where
    askConfig :: m AgentConfig

instance (Monad m) => ConfigReader (ReaderT AgentConfig m) where
    askConfig = ask


newtype Agent a = Agent { unAgent :: ReaderT AgentConfig (LevelDBT Process) a}
            deriving ( Functor, Applicative, Monad, MonadBase IO
                     , ConfigReader, MonadIO, MonadThrow, MonadUnsafeIO
                     , MonadResource, MonadLevelDB
                     )
instance MonadProcess Agent where
    liftProcess ma = Agent $ lift $ lift ma
