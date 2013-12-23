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
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Types
 ( module FreeAgent.Types
 , Storeable, MonadLevelDB, Key, Value, KeySpace
 , MonadProcess
 , Serializable
 , MonadBase
 , MonadBaseControl
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

import           Control.Monad.Logger  (LoggingT, MonadLogger)
import           Control.Monad.Writer (Writer)

import           Control.Concurrent.Chan.Lifted (Chan)
import          Database.LevelDB.Higher
    ( LevelDB, MonadLevelDB,Storeable, Key, Value, KeySpace
    , FetchFail(..))

import           Control.Distributed.Process.Lifted
import           Control.Monad.Base (MonadBase)
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
data Action = forall a b. (Actionable a b) => Action a

instance Typeable Action where
    typeOf _ = mkTyConApp (mkTyCon3 "free-agent" "FreeAgent.Types" "Action") []

instance P.Show Action where
    show (Action a) = "Action (" ++ P.show a ++ ")"


-- | Class for types that will be boxed as ActionResult
class (Serializable a, Stashable a) => Resulting a where
    -- | extract the concrete result - if you know what type it is
    extract :: (Typeable b) => a -> Maybe b
    -- | provide a 'ResultSummary'
    summary :: a -> ResultSummary
    matchResult :: (Typeable b) => (b -> Bool) -> a -> Bool

    matchResult f a =
        case cast a of
            Just a' -> f a'
            Nothing -> False
    extract = cast

-- ActionResult
-- | Box for returning results from 'Action' exec.
--
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
type ActionListener = (Action -> Bool, ProcessId)
type ResultListener = (ActionResult -> Bool, ProcessId)

data DBMessage = Perform (AgentDB ()) | Terminate

data PluginDef
  = PluginDef { _plugindefName :: !ByteString
              , _plugindefContext :: !Dynamic
              , _plugindefActions :: ![PluginActions]
              , _plugindefActionListeners :: Agent [ActionListener]
              , _plugindefResultListeners :: Agent [ResultListener]
              }

data AgentConfig
  = AgentConfig  { _configDbPath :: !FilePathS
                 , _configNodeHost :: !String
                 , _configNodePort :: !String
                 , _configPluginContexts :: !PluginContexts
                 }

data AgentContext
  = AgentContext { _contextActionMap :: !ActionMap
                 , _contextResultMap :: !ResultMap
                 , _contextAgentConfig :: !AgentConfig
                 , _contextAgentDBChan :: Chan DBMessage
                 , _contextActionListeners :: Agent [ActionListener]
                 , _contextResultListeners :: Agent [ResultListener]
                 }

instance Default AgentConfig where
    def = AgentConfig "./db" "127.0.0.1" "3546" mempty

instance Default AgentContext where
    def = AgentContext mempty mempty def
            (error "agentDB chan not initialized!")
            (return [])
            (return [])

class (Functor m, Applicative m, Monad m)
      => ConfigReader m where
    askConfig :: m AgentContext

instance (Functor m, Applicative m,Monad m)
         => ConfigReader (ReaderT AgentContext m) where
    askConfig = ask

-- Agent Monad

type AgentDB m = LevelDB m

type AgentBase m = (Applicative m, Monad m, MonadIO m, MonadBase IO m, MonadBaseControl IO m)
type MonadAgent m = (AgentBase m, ConfigReader m, MonadProcess m)

newtype Agent a = Agent { unAgent :: ReaderT AgentContext (LoggingT Process) a}
            deriving ( Functor, Applicative, Monad, MonadBase IO
                     , ConfigReader, MonadIO, MonadLogger
                     )
instance MonadBaseControl IO Agent where
  newtype StM Agent a = StAgent {unSTAgent :: StM (ReaderT AgentContext (LoggingT Process)) a}
  restoreM (StAgent m) = Agent $ restoreM m
  liftBaseWith f = Agent $ liftBaseWith $ \ rib -> f (fmap StAgent . rib . unAgent)

instance MonadProcess Agent where
    liftProcess ma = Agent . lift $ lift ma
    mapProcess f ma = Agent $
        mapReaderT (mapLoggingT f) (unAgent ma)
      where
        mapLoggingT f' = lift . f' . runAgentLoggingT

data RunStatus a = Either Text a
    deriving (Show, Eq)

class (Serializable a, Serializable b, Stashable a, Stashable b, Resulting b)
     => Runnable a b | a -> b where
    exec :: (MonadAgent m) => a -> m (Either Text b)
    matchAction :: (Typeable c) => (c -> Bool) -> a -> Bool

    matchAction f a =
        case cast a of
            Just a' -> f a'
            Nothing -> False

data ResultSummary
  = ResultSummary { _resultTimestamp :: UTCTime
                  , _resultText :: Text
                  , _resultResultOf :: Action
                  } deriving (Show, Typeable, Generic)

instance Cereal.Serialize UTCTime where
    get = do
        stime <- Cereal.get :: Cereal.Get ByteString
        return $ bytesToUtc stime
    put = Cereal.put . utcToBytes

instance Binary UTCTime where
    get = do
        stime <- get :: Get ByteString
        return $ bytesToUtc stime
    put = put . utcToBytes

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
