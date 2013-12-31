{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Types
 ( module FreeAgent.Types
 , Storeable, MonadLevelDB, Key, Value, KeySpace
 , MonadProcess
 , NFSerializable
 , MonadBase
 , MonadBaseControl
 , NFData(..)
 )

where

import           FreeAgent.Prelude
import qualified Prelude                            as P

import           Control.Monad.Reader               (ReaderT, ask, mapReaderT)
import           Control.Monad.Writer               (Writer)
import           Data.Binary                        as Binary
import           Data.Dynamic                       (Dynamic)
import           Data.Typeable                      (cast)

import           Control.Concurrent.Chan.Lifted     (Chan)
import           Control.DeepSeq                    (NFData (..))
import           Control.DeepSeq.TH                 (deriveNFData)
import           Control.Distributed.Process.Lifted
import           Control.Distributed.Process.Node   (LocalNode)
import           Control.Monad.Base                 (MonadBase)
import           Control.Monad.Logger               (LogLevel (..), LoggingT,
                                                     MonadLogger (..),
                                                     runStdoutLoggingT,
                                                     withChannelLogger)
import           Control.Monad.Trans.Control
import           Data.Default                       (Default (..))
import           Data.SafeCopy                      (base, deriveSafeCopy)
import qualified Data.Serialize                     as Cereal (Get,
                                                               Serialize (..),
                                                               encode)
import           Data.Time.Clock
import           Database.LevelDB.Higher            (FetchFail (..), Key,
                                                     KeySpace, LevelDBT,
                                                     MonadLevelDB, Storeable,
                                                     Value)


-- | Types that can be serialized, stored and retrieved
--
class (Storeable a) => Stashable a where
    key :: a -> Key

-- Wrapped
-- | Wrapped lets us store an Action and recover it using
-- TODO: fix this comment: the type name in 'registerUnwrappers'
data Wrapped
  = Wrapped { _wrappedWrappedKey :: ByteString
            , _wrappedTypeName   :: ByteString
            , _wrappedValue      :: ByteString
            } deriving (Show, Eq, Typeable, Generic)
deriveSafeCopy 1 'base ''Wrapped
deriveNFData ''Wrapped

instance Binary Wrapped where

-- we cannot use safeGet/safePut with decodeAction' presently
-- so we can't use deriveSerializable
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

type Actionable a b = (Stashable a, Stashable b, Resulting b, Runnable a b, NFData a, NFData b)

data Action = forall a b. (Actionable a b) => Action a

deriving instance Typeable Action

instance P.Show Action where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance NFData Action where
    rnf (Action a) = rnf a


-- | Class for types that will be boxed as ActionResult
class (NFSerializable a, Stashable a) => Resulting a where
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

deriving instance Typeable ActionResult

instance NFData ActionResult where
    rnf (ActionResult a) = rnf a



instance NFData FetchFail

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
  = PluginDef { _plugindefName            :: !ByteString
              , _plugindefContext         :: !Dynamic
              , _plugindefActions         :: ![PluginActions]
              , _plugindefActionListeners :: Agent [ActionListener]
              , _plugindefResultListeners :: Agent [ResultListener]
              }

data AgentConfig
  = AgentConfig  { _configDbPath         :: !FilePathS
                 , _configNodeHost       :: !String
                 , _configNodePort       :: !String
                 , _configPluginContexts :: !PluginContexts
                 , _configDebugLogCount  :: !Int
                 , _configMinLogLevel    :: !LogLevel
                 }

data AgentContext
  = AgentContext { _contextActionMap       :: !ActionMap
                 , _contextResultMap       :: !ResultMap
                 , _contextAgentConfig     :: !AgentConfig
                 , _contextAgentDBChan     :: Chan DBMessage
                 , _contextActionListeners :: Agent [ActionListener]
                 , _contextResultListeners :: Agent [ResultListener]
                 , _contextProcessNode     :: LocalNode
                 }

instance Default AgentConfig where
    def = AgentConfig "./db" "127.0.0.1" "3546" mempty 10 LevelWarn

instance Default AgentContext where
    def = AgentContext mempty mempty def
            (error "agentDB chan not initialized!")
            (return [])
            (return [])
            (error "process node not initialized!")

class (Functor m, Applicative m, Monad m)
      => ConfigReader m where
    askConfig :: m AgentContext

instance (Functor m, Applicative m,Monad m)
         => ConfigReader (ReaderT AgentContext m) where
    askConfig = ask

-- Agent Monad

type AgentDB m = LoggingT (LevelDBT IO) m

type AgentBase m = (Applicative m, Monad m, MonadIO m, MonadBase IO m, MonadBaseControl IO m)
type MonadAgent m = (AgentBase m, ConfigReader m, MonadProcess m, MonadLogger m)

newtype Agent a = Agent { unAgent :: ReaderT AgentContext (LoggingT Process) a}
            deriving ( Functor, Applicative, Monad, MonadBase IO
                     , ConfigReader, MonadIO
                     )
instance MonadBaseControl IO Agent where
  newtype StM Agent a = StAgent {unSTAgent :: StM (ReaderT AgentContext (LoggingT Process)) a}
  restoreM (StAgent m) = Agent $ restoreM m
  liftBaseWith f = Agent $ liftBaseWith $ \ rib -> f (fmap StAgent . rib . unAgent)

instance MonadLogger (Agent) where
    monadLoggerLog a b level d =
        (_configMinLogLevel . _contextAgentConfig) <$> askConfig >>= doLog
      where doLog minlev
              | level >= minlev = runStdoutLoggingT $ monadLoggerLog a b level d
              | otherwise = Agent $ lift $ monadLoggerLog a b LevelDebug d

runAgentLoggingT :: (MonadIO m, MonadBaseControl IO m) => Int -> LoggingT m a -> m a
runAgentLoggingT debugCount = runStdoutLoggingT . withChannelLogger debugCount

instance MonadProcess Agent where
    liftProcess ma = Agent . lift $ lift ma
    mapProcess f ma = Agent $ do
        debugCount <- (_configDebugLogCount . _contextAgentConfig) <$> askConfig
        mapReaderT (mapLoggingT debugCount f) (unAgent ma)
      where
        mapLoggingT conf f' = lift . f' . runAgentLoggingT conf


data RunStatus a = Either Text a
    deriving (Show, Eq)

class (NFSerializable a, NFSerializable b, Stashable a, Stashable b, Resulting b)
     => Runnable a b | a -> b where
    exec :: (MonadAgent m) => a -> m (Either Text b)
    matchAction :: (Typeable c) => (c -> Bool) -> a -> Bool

    matchAction f a =
        case cast a of
            Just a' -> f a'
            Nothing -> False

data ResultSummary
  = ResultSummary { _resultTimestamp :: UTCTime
                  , _resultText      :: Text
                  , _resultResultOf  :: Action
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

