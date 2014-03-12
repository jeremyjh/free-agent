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
 , MonadLevelDB, Key, Value, KeySpace
 , MonadProcess
 , NFSerializable, Process, ProcessId
 , MonadBase
 , MonadBaseControl
 , NFData(..)
 , LogLevel(..), MonadLogger
 , UUID
 )

where

import           AgentPrelude
import qualified Prelude                            as P

import           Control.Monad.Reader               (ReaderT, ask, mapReaderT)
import           Control.Monad.Writer               (Writer)
import           Data.Binary                        as Binary
import           Data.Dynamic                       (Dynamic)
import           Data.Time.Clock                    (UTCTime)
import           Data.Typeable                      (cast)

import           Control.Concurrent.Chan.Lifted     (Chan)
import           Control.DeepSeq                    (NFData (..))
import           Control.Error                      (EitherT)
import           FreeAgent.Process (MonadProcess(..), Process
                                                    ,ProcessId, NFSerializable
                                                    ,ChildSpec, RemoteTable)
import           Control.Distributed.Process.Node   (LocalNode, initRemoteTable)
import           Control.Distributed.Process.Internal.Types (LocalProcessId)
import Control.Distributed.Process (NodeId, Closure)
import           Control.Monad.Base                 (MonadBase)
import           Control.Monad.Logger               (LogLevel (..), LoggingT,
                                                     MonadLogger (..),
                                                     runStdoutLoggingT,
                                                     withChannelLogger)
import           Control.Monad.Trans.Control
import Control.Monad.State (StateT)
import           Data.Default                       (Default (..))
import qualified Data.Set                           as Set
import           Data.UUID                          (UUID)
import Data.SafeCopy
       (SafeCopy(..), Contained, contain, safePut, safeGet, base)
import           Data.Serialize                     (Serialize)
import qualified Data.Serialize                     as Cereal
import           Database.LevelDB.Higher            (Key, KeySpace, LevelDBT
                                                    ,MonadLevelDB
                                                    ,Value)
import           Network.Transport (EndPointAddress)


type SafeStore a = (SafeCopy a, Serialize a, Show a, Typeable a)

-- | Types that can be serialized, stored and retrieved
--
class (SafeStore a) => Stashable a where
    key :: a -> Key

-- Wrapped
-- | Wrapped lets us store an Action and recover it using
-- TODO: fix this comment: the type name in 'registerUnwrappers'
data Wrapped
  = Wrapped { _wrappedWrappedKey :: ByteString
            , _wrappedTypeName   :: ByteString
            , _wrappedValue      :: ByteString
            } deriving (Show, Eq, Typeable, Generic)


instance Stashable Wrapped where
    key = _wrappedWrappedKey

type Actionable a b = (Stashable a, Stashable b, Resulting b, Runnable a b, NFData a, NFData b, Eq a)

data Action = forall a b. (Actionable a b) => Action a

deriving instance Typeable Action

instance Eq Action where
    (Action a) == (Action b) = maybe False (a ==) (cast b)

instance Ord Action where
    (Action a) `compare` (Action b) = key a `compare` key b

instance P.Show Action where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance NFData Action where
    rnf (Action a) = rnf a


-- | Class for types that will be boxed as Result
class (NFSerializable a, Stashable a) => Resulting a where
    -- | extract the concrete result - if you know what type it is
    extract :: (Typeable b) => a -> Maybe b
    -- | provide a 'ResultSummary'
    summary :: a -> ResultSummary
    -- | Create a generalized ResultMatcher function - see 'Core.resultMatcher'
    matchR :: (Typeable b) => (b -> Bool) -> a -> Bool

    matchR f a = maybe False f (cast a)
    extract = cast

-- Result
-- | Box for returning results from 'Action' exec.
--
data Result = forall a. (Resulting a, Show a) => Result a

instance Show Result where
    show (Result a) = show a

instance Eq Result where
    (Result a) == (Result b) = Binary.encode a == Binary.encode b

deriving instance Typeable Result

instance NFData Result where
    rnf (Result a) = rnf a

type FetchAction = Either String Action
-- Unwrapper and registration types
type Unwrapper a = (Wrapped -> Either String a)
type ActionMap = Map ByteString (Unwrapper Action)
type ResultMap = Map ByteString (Unwrapper Result)
type PluginMaps = (ActionMap, ResultMap)
type PluginContexts = Map ByteString Dynamic
type PluginActions = ( ByteString, Unwrapper Action
                     , ByteString, Unwrapper Result)
type PluginWriter = Writer [PluginDef] ()
type ActionsWriter = Writer [PluginActions] ()
type ActionMatcher = (Action -> Bool)
type ResultMatcher = (Result -> Bool)

data Listener = ActionMatching ActionMatcher NodeId String
              | ResultMatching ActionMatcher ResultMatcher NodeId String
              deriving (Typeable)

data DBMessage = Perform (AgentDB ()) | Terminate

-- | Dynamic wrapper around an open 'AcidState' and a function
-- which will close it - used with functions from
-- 'FreeAgent.Database.AcidState'


data StateHandles =
    StateHandles { acState :: Dynamic
                 , acClose :: () -> IO ()
                 }

data PluginDef
  = PluginDef { _plugindefName      :: !ByteString
              , _plugindefContext   :: !Dynamic
              , _plugindefActions   :: ![PluginActions]
              , _plugindefListeners :: Agent [Listener]
              }

-- | AgentConfig data is set at startup and does not change while
-- the process is running
data AgentConfig
  = AgentConfig  { _configDbPath         :: !FilePathS
                 , _configNodeHost       :: !String
                 , _configNodePort       :: !String
                 , _configPluginContexts :: !PluginContexts
                 , _configDebugLogCount  :: !Int
                 , _configMinLogLevel    :: !LogLevel
                 , _configContexts       :: Set Context
                 , _configZones          :: Set Zone
                 , _configPeerNodeSeeds  :: [String]
                 }

-- | Each agent belongs to one or more contexts - every 'Package' specifies
-- the Context(s) in which it will execute
data Context = Context !Text deriving (Show, Eq, Ord, Typeable, Generic)

instance Default Context where def = Context "default"

instance Default (Set Context) where def = Set.fromList [def]

-- | Each agent belongs to one or more Zones - functionally this is the
-- similar to a Context but it is intended to indicate geographic or
-- network location (e.g. Zone "BehindFirewall", Zone "DMZ", Zone "Public")
data Zone = Zone !Text deriving (Show, Eq, Ord, Typeable, Generic)

instance Default Zone where def = Zone "default"

-- | AgentContext may be initialized during startup but is more
-- dynamic than config and may change and/or provide communications
data AgentContext
  = AgentContext { _contextActionMap       :: !ActionMap
                 , _contextResultMap       :: !ResultMap
                 , _contextAgentConfig     :: !AgentConfig
                 , _contextAgentDBChan     :: Chan DBMessage
                 , _contextListeners       :: Agent [Listener]
                 , _contextProcessNode     :: LocalNode
                 , _contextRemoteTable     :: RemoteTable
                 , _contextOpenStates      :: MVar (Map String StateHandles)
                 }

instance Default AgentConfig where
    def = AgentConfig "./db" "127.0.0.1" "3546" mempty 10 LevelWarn
            (Set.fromList [Context "default"])
            (Set.fromList [Zone "default"])
            []

instance Default AgentContext where
    def = AgentContext mempty mempty def
            (error "agentDB chan not initialized!")
            (return [])
            (error "process node not initialized!")
            initRemoteTable
            (error "states mvar not initialized! ")

class (Functor m, Applicative m, Monad m)
      => ContextReader m where
    askContext :: m AgentContext

instance (Functor m, Applicative m,Monad m)
         => ContextReader (ReaderT AgentContext m) where
    askContext = ask

instance (ContextReader m)
      => ContextReader (StateT a m) where
    askContext = lift askContext

instance ContextReader m => ContextReader (EitherT e m) where
    askContext = lift askContext

-- Agent Monad

type AgentDB m = LoggingT (LevelDBT IO) m

type AgentBase m = (Applicative m, Monad m, MonadIO m, MonadBase IO m, MonadBaseControl IO m)
type MonadAgent m = (AgentBase m, ContextReader m, MonadProcess m, MonadLogger m)

newtype Agent a = Agent { unAgent :: ReaderT AgentContext (LoggingT Process) a}
            deriving ( Functor, Applicative, Monad, MonadBase IO
                     , ContextReader, MonadIO
                     )
instance MonadBaseControl IO Agent where
  newtype StM Agent a = StAgent {unSTAgent :: StM (ReaderT AgentContext (LoggingT Process)) a}
  restoreM (StAgent m) = Agent $ restoreM m
  liftBaseWith f = Agent $ liftBaseWith $ \ rib -> f (fmap StAgent . rib . unAgent)

instance MonadLogger (Agent) where
    monadLoggerLog a b level d =
        (_configMinLogLevel . _contextAgentConfig) <$> askContext >>= doLog
      where doLog minlev
              | level >= minlev = runStdoutLoggingT $ monadLoggerLog a b level d
              | otherwise = return ()

runAgentLoggingT :: (MonadIO m, MonadBaseControl IO m) => Int -> LoggingT m a -> m a
runAgentLoggingT debugCount = runStdoutLoggingT . withChannelLogger debugCount

instance MonadProcess Agent where
    liftProcess ma = Agent . lift $ lift ma
    mapProcess f ma = Agent $ do
        debugCount <- (_configDebugLogCount . _contextAgentConfig) <$> askContext
        mapReaderT (mapLoggingT debugCount f) (unAgent ma)
      where
        mapLoggingT conf f' = lift . f' . runAgentLoggingT conf



-- | Failure modes for exec method of Runnable
-- A failure should mean that the action could not
-- complete execution - negative results of successful execution
-- should be captured in the Resulting type
data RunnableFail =
                  -- | Action-specific general failure to exec
                     GeneralFailure Text
                  -- | An unhandled IOException message
                  | RIOException Text
                  -- | Could not execute due to missing (local) dependency
                  -- (e.g. 'ruby' not found)
                  | DependencyFailure Text
                  -- | Could not execute due to unavailable network dependency
                  -- (e.g. could not reach database server)
                  | NetworkFailure Text
                  -- | Dependency response unexpected
                  | UnknownResponse Text
    deriving (Show, Eq, Typeable, Generic)

class (NFSerializable a, NFSerializable b, Stashable a, Stashable b, Resulting b)
     => Runnable a b | a -> b where
    exec :: (MonadAgent m) => a -> m (Either RunnableFail b)
    -- | Create a generalized ActionMatcher function - see 'Core.actionMatcher'
    matchA :: (Typeable c) => (c -> Bool) -> a -> Bool

    matchA f a = maybe False f (cast a)


data ResultSummary
  = ResultSummary { _resultTimestamp :: UTCTime
                  , _resultText      :: Text
                  , _resultResultOf  :: Action
                  } deriving (Show, Typeable, Generic)


-- Scheduling
data Schedule = Now | Later
    deriving (Show, Eq, Typeable, Generic)

data ActionHistory = ActionHistory deriving (Show, Eq, Typeable, Generic)

data AgentServer = AgentServer { _serverName :: String
                               , _serverinitProc :: AgentContext -> Process ()
                               , _serverchildSpec :: AgentContext -> Process ChildSpec
                               }

data ServerRef = ServerRef String ProcessId
                 deriving (Show, Eq, Generic)

instance Ord ServerRef where
    ServerRef a _ `compare` ServerRef b _ = a `compare` b

data Peer = Peer { _peerUuid      :: UUID
                 , _peerProcessId :: !ProcessId
                 , _peerContexts  :: Set Context
                 , _peerZones     :: Set Zone
                 , _peerServers   :: Set ServerRef
                 } deriving (Show, Eq, Typeable, Generic)

instance Ord Peer where
    a `compare` b = _peerUuid a `compare` _peerUuid b

data Target =   Local
              | Remote Peer
              | Route [Context] [Zone]

-- create safecopy instances for Binary types - this
-- is unsafe since migrations are impossible
class Binary a => UnsafeCopy a where
    unsafeGet :: Contained (Cereal.Get a)
    unsafePut :: a -> Contained Cereal.Put

    unsafeGet = contain $ safeGet >>= return . Binary.decode
    unsafePut = contain . safePut . Binary.encode

instance Typeable a => UnsafeCopy (Closure a)
instance Typeable a => SafeCopy (Closure a) where
    version = 1
    kind = base
    errorTypeName _ = "Control.Distributed.Static.Closure"
    putCopy = unsafePut
    getCopy = unsafeGet

instance Typeable a => Serialize (Closure a) where
    get = safeGet
    put = safePut

deriveSafeStore ''UUID
deriveSerializers ''Context
deriveSerializers ''Zone
deriveSerializers ''Wrapped
deriveSerializers ''Schedule
deriveSerializers ''RunnableFail
deriveSerializers ''ServerRef
deriveSerializers ''Peer

deriveSafeStore ''LocalProcessId
deriveSafeStore ''EndPointAddress
deriveSafeStore ''NodeId
deriveSafeStore ''ProcessId
