{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving#-}
{-# LANGUAGE DeriveGeneric#-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Types
 ( module FreeAgent.Types
 , Storeable, MonadLevelDB, Key, Value, KeySpace
 , MonadProcess
 , Serializable
 )

where

import           FreeAgent.Prelude
import qualified Prelude              as P
import           Control.Monad.Reader (ReaderT, mapReaderT, ask)

import           Data.Typeable        (mkTyConApp, mkTyCon3, cast)
import           Data.Default         (Default(..))
import           Data.Dynamic         (Dynamic)

-- yes we have both cereal and binary ... cereal for safecopy
-- binary for distributed-process
import qualified Data.Serialize       as Cereal
    (Serialize(..), encode, Get)
import           Data.Binary          as Binary
import           Data.SafeCopy        (deriveSafeCopy, base)
import           Control.Distributed.Process.Serializable (Serializable)

import           Control.Monad.Writer (Writer)

import          Database.LevelDB.Higher
    (LevelDBT, MonadLevelDB,Storeable, Key, Value, KeySpace
    , mapLevelDBT, FetchFail(..))

import           Control.Distributed.Process.Lifted
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO, MonadResource)
import           Control.Monad.Trans.Control
import           Data.Time.Clock


-- | Types that can be serialized, stored and retrieved
--
class (Storeable a) => Stashable a where
    key :: a -> Key

-- Wrapped
-- | Wrapped lets us store an Action and recover it using
-- TODO: fix this comment: the type name in 'registerUnwrappers'
data Wrapped
  = Wrapped { _wrappedWrappedKey :: ByteString
                  , _wrappedTypeName :: ByteString
                  , _wrappedValue :: ByteString
                  }
    deriving (Show, Eq, Typeable, Generic)
deriveSafeCopy 1 'base ''Wrapped
instance Binary Wrapped where
-- so, we need Cereal to implement SafeCopy so it is Storable and
-- Stashable - yet we cannot use safeGet/safePut with decodeAction' presently
instance Cereal.Serialize Wrapped where
        put (Wrapped x1 x2 x3) = do
            Cereal.put x1
            Cereal.put x2
            Cereal.put x3
        get = do
            x1 <- Cereal.get
            x2 <- Cereal.get
            x3 <- Cereal.get
            return (Wrapped x1 x2 x3)

instance Stashable Wrapped where
    key = _wrappedWrappedKey

type Actionable a b = (Stashable a, Stashable b, Resulting b, Runnable a b)
data Action = forall a b. (Actionable a b) => Action a b

instance Typeable Action where
    typeOf _ = mkTyConApp (mkTyCon3 "free-agent" "FreeAgent.Types" "Action") []

instance P.Show Action where
    show (Action a _) = "Action (" ++ P.show a ++ ")"

-- | Class for types that will be boxed as ActionResult
-- This let's us use the UnsafePrimitive's version of 'send' which
-- sends a message locally without serializing it
class (Serializable a, Stashable a) => Resulting a where
    deliver :: (MonadProcess m) => a -> ProcessId -> m ()
    extract :: (Typeable b) => a -> Maybe b
    summary :: a -> ResultSummary

    -- | send the result to the listener argument
    deliver a p = send p a
    -- | extract the concrete result - if you know what type it is
    extract = cast

-- ActionResult
-- | Box for returning results from 'Action' exec.
-- Box is an existential which implements Deliverable and thus can be
-- sent as a concrete type to registered listeners of the Action
--
-- use the 'actionResult' and 'extractResult' functions from FreeAgent.Action
data ActionResult = forall a. (Resulting a, Show a) => ActionResult a

instance Show ActionResult where
    show (ActionResult a) = show a

instance Eq ActionResult where
    (ActionResult a) == (ActionResult b) = Binary.encode a == Binary.encode b

instance Typeable ActionResult where
    typeOf _ = mkTyConApp (mkTyCon3 "free-agent" "FreeAgent.Types" "ActionResult") []

type FetchAction = Either FetchFail Action

-- Unwrapper and registration types
type Unwrapper a = (Wrapped -> Either FetchFail a)
type ActionMap = Map ByteString (Unwrapper Action)
type ResultMap = Map ByteString (Unwrapper ActionResult)
type PluginMaps = (ActionMap, ResultMap)
type PluginContexts = Map ByteString Dynamic
type PluginActions = ( ByteString, Unwrapper Action
                     , ByteString, Unwrapper ActionResult)
type PluginWriter = Writer [PluginDef] ()
type ActionsWriter = Writer [PluginActions] ()

data PluginDef
  = PluginDef { _plugindefName :: ByteString
              , _plugindefContext :: Dynamic
              , _plugindefActions :: [PluginActions]
              }

data AgentConfig
  = AgentConfig  { _configDbPath :: FilePathS
                 , _configNodeHost :: String
                 , _configNodePort :: String
                 , _configPluginContexts :: PluginContexts
                 }

data AgentContext
  = AgentContext { _contextActionMap :: ActionMap
                 , _contextResultMap :: ResultMap
                 , _contextAgentConfig :: AgentConfig
                 }

instance Default AgentConfig where
    def = AgentConfig "./db" "127.0.0.1" "3546" mempty

instance Default AgentContext where
    def = AgentContext mempty mempty def

class (Monad m) => ConfigReader m where
    askConfig :: m AgentContext

instance (Monad m) => ConfigReader (ReaderT AgentContext m) where
    askConfig = ask

-- Agent Monad
type MonadAgent m = (ConfigReader m, MonadProcess m, MonadLevelDB m, MonadBaseControl IO m)

newtype Agent a = Agent { unAgent :: ReaderT AgentContext (LevelDBT Process) a}
            deriving ( Functor, Applicative, Monad, MonadBase IO
                     , ConfigReader, MonadIO, MonadThrow, MonadUnsafeIO
                     , MonadResource, MonadLevelDB
                     )
instance MonadBaseControl IO Agent where
  newtype StM Agent a = StAgent {unSTAgent :: StM (ReaderT AgentContext (LevelDBT Process)) a}
  restoreM (StAgent m) = Agent $ restoreM m
  liftBaseWith f = Agent $ liftBaseWith $ \ rib -> f (fmap StAgent . rib . unAgent)

instance MonadProcess Agent where
    liftProcess ma = Agent $ lift $ lift ma
    mapProcess f ma = Agent $
        mapReaderT (mapLevelDBT f) (unAgent ma)

data RunStatus a = Either Text a
    deriving (Show, Eq)

class (Serializable b, Show b) => Runnable a b | a -> b where
    exec :: (MonadAgent m) => a -> m (Either Text b)

data ResultSummary
  = ResultSummary { _resultTimestamp :: UTCTime
                  , _resultText :: Text
                  } deriving (Eq, Show, Typeable, Generic)

instance Cereal.Serialize UTCTime where
    get = do
        stime <- Cereal.get :: Cereal.Get String
        return $ P.read stime
    put a = Cereal.put (show a)

instance Binary UTCTime where
    get = do
        stime <- get :: Get String
        return $ P.read stime
    put a = put (show a)

deriveSerializers ''ResultSummary

-- Scheduling and Events
data Schedule = Now | Later
    deriving (Show, Eq, Typeable, Generic)
deriveSerializers ''Schedule

data Event = Event Schedule Wrapped
    deriving (Show, Eq, Typeable, Generic)
deriveSerializers ''Event

instance Stashable Event where
    key (Event sch act)
      = Cereal.encode sch ++ key act

data ActionHistory = ActionHistory deriving (Show, Eq, Typeable, Generic)

data EventHistory = EventHistory deriving (Show, Eq, Typeable, Generic)
