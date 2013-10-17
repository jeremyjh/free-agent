{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Dash.Types where

import           Dash.Prelude
import qualified Prelude              as P
import           Control.Monad.Reader (ReaderT, ask)

import           Data.Typeable        (mkTyConApp, mkTyCon3)
import           Data.SafeCopy
import           Data.Serialize
import           Data.Default

import           Dash.Store           (Stashable(..), FetchFail)
import           Control.Monad.Writer (Writer)


data RunStatus = Running (Maybe String)
               | Complete (Maybe String)
               | Failed (Maybe String)
    deriving (Show, Eq)

class Runnable a where
    exec :: a -> IO RunStatus


-- | Wrapper lets us store an Actionnd recover it using
-- the type name in 'registerUnWrappers'
data Wrapped = Wrapped { wType :: ByteString
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

data AgentConfig = AgentConfig { _configPlugins :: PluginMap }

instance Default AgentConfig where
    def = AgentConfig empty

makeFields ''AgentConfig

class (Monad m) => ConfigReader m where
    askConfig :: m AgentConfig

instance (Monad m) => ConfigReader (ReaderT AgentConfig m) where
    askConfig = ask

viewConfig :: (ConfigReader m) => Getting a AgentConfig a -> m a
viewConfig lens = do
    conf <- askConfig
    return $ view lens conf
