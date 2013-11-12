{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}


module FreeAgent.Types
 ( module FreeAgent.Types
 , Storeable, MonadLevelDB, Key, Value
 , MonadProcess
 , Serializable
 )

where

import           FreeAgent.Prelude
import qualified Prelude              as P
import           Control.Monad.Reader (ReaderT, ask)

import           Data.Typeable        (mkTyConApp, mkTyCon3)
import           Data.SafeCopy
    (deriveSafeCopy, SafeCopy(..), base, safeGet, safePut)
import           Data.Default         (Default(..))
import           Data.Dynamic         (Dynamic, toDyn)

-- yes we have both cereal and binary ... cereal for safecopy
-- binary for distributed-process
import           Data.Serialize       as Cereal (Serialize(..), encode)
import           Data.Binary          as Binary (Binary(..), encode)
import           Control.Distributed.Process.Serializable (Serializable)

import           Control.Monad.Writer (Writer)

import          Database.LevelDB.Higher
    (LevelDBT, MonadLevelDB,Storeable, Key, Value, FetchFail)

import           Control.Distributed.Process.Lifted
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO, MonadResource)
import           Control.Monad.Trans.Control



-- | Types that can be serialized, stored and retrieved
--
class (Storeable a) => Stashable a where
    key :: a -> Key

-- | Wrapper lets us store an Action and recover it using
-- the type name in 'registerUnWrappers'
data Wrapped
  = Wrapped { _wrappedWrappedKey :: ByteString
            , _wrappedTypeName :: ByteString
            , _wrappedValue :: ByteString
            }
    deriving (Show, Typeable)

-- Wrap a concrete type for stash or send where it
-- will be decoded to an Action
wrap :: (Stashable a) => a -> Wrapped
wrap st = Wrapped (key st) (fqName st) (Cereal.encode st)

deriveSafeCopy 1 'base ''Wrapped

instance Serialize Wrapped where
    get = safeGet
    put = safePut

instance Stashable Wrapped where
    key w = _wrappedWrappedKey w

type Actionable a b = (Stashable a, Runnable a b)
data Action = forall p b. (Actionable p b) => Action p b

instance Cereal.Serialize Action where
    put (Action a _) = Cereal.put $ wrap a
    get = error "decode/get directly on Action can't happen; use decodeAction"

instance SafeCopy Action where
    putCopy (Action a _) = putCopy a

instance Eq Action where
    a == b =  Cereal.encode a == Cereal.encode b

instance Stashable Action where
    key (Action a _) = key a

instance Typeable Action where
    typeOf _ = mkTyConApp (mkTyCon3 "free-agent" "FreeAgent.Types" "Action") []

instance P.Show Action where
    show (Action a _) = "Action (" ++ P.show a ++ ")"

instance Runnable Action ActionResult where
    exec (Action a _) = do
        (Complete result) <- exec a
        return $ Complete (ActionResult result (toDyn result))

type FetchAction = Either FetchFail Action

type UnWrapper a = (Wrapped -> Either FetchFail a)
type ActionMap = Map ByteString (UnWrapper Action)
type PluginContexts = Map ByteString Dynamic
type PluginActions = (ByteString, UnWrapper Action)
type PluginWriter = Writer [PluginDef] ()
type ActionsWriter = Writer [PluginActions] ()

data PluginDef
  = PluginDef { _plugindefName :: ByteString
              , _plugindefContext :: Dynamic
              , _plugindefActions :: [PluginActions]
              }

data AgentContext
  = AgentContext { _configActionMap :: ActionMap
                 , _configPluginContexts :: PluginContexts
                 , _configDbPath :: FilePathS
                 , _configNodeHost :: String
                 , _configNodePort :: String
                 }

instance Default AgentContext where
    def = AgentContext mempty mempty "./db" "127.0.0.1" "3546"

class (Monad m) => ConfigReader m where
    askConfig :: m AgentContext

instance (Monad m) => ConfigReader (ReaderT AgentContext m) where
    askConfig = ask

type MonadAgent m = (ConfigReader m, MonadProcess m, MonadLevelDB m)

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

data RunStatus a = Running ProcessId
                   | Complete a
                   | Failed Text
    deriving (Show, Eq)

class (Serializable b, Show b) => Runnable a b | a -> b where
    exec :: a -> Agent (RunStatus b)

-- | Box for returning results from 'Action' exec.
-- Box is an existential which implements Deliverable and thus can be
-- sent as a concrete type to registered listeners of the Action
--
-- use the 'actionResult' and 'extractResult' functions from FreeAgent.Action
data ActionResult = forall p. (Serializable p, Show p) => ActionResult p Dynamic

instance Show ActionResult where
    show (ActionResult a _) = show a

instance Eq ActionResult where
    (ActionResult a _) == (ActionResult b _) = Binary.encode a == Binary.encode b

instance Typeable ActionResult where
    typeOf _ = mkTyConApp (mkTyCon3 "free-agent" "FreeAgent.Types" "ActionResult") []

instance Binary ActionResult where
    put (ActionResult a _) = Binary.put a
    get = error "You cannot decode to the ActionResult existential type."

-- | Class for types that will be boxed as ActionResult
-- This let's us use the UnsafePrimitive's version of 'send' which
-- sends a message locally without serializing it
class (Serializable a) => Resulting a where
    deliver :: (MonadProcess m) => a -> ProcessId -> m ()
    extract :: a -> Dynamic

    -- | send the result to the listener argument
    deliver a p = send p a
    -- | extract the embedded dynamic; for use by 'extractResult'
    extract = toDyn

-- ActionResult instance overrides the underlying implementation
-- there is no reason for an different implementation to ever be used than
-- the default, but this ensures ActionResults are all sent the same way
instance Resulting ActionResult where
    deliver (ActionResult a _) p = send p a
    extract (ActionResult _ d) = d
