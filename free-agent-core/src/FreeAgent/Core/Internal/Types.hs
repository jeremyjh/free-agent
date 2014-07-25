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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module FreeAgent.Core.Internal.Types
 ( module FreeAgent.Core.Internal.Types
 , MonadProcess
 , NFSerializable, Process, ProcessId
 , MonadBase
 , MonadBaseControl
 , NFData(..)
 , LogLevel(..), MonadLogger
 , UUID
 , FromJSON(..), ToJSON (..)
 )

where

import           AgentPrelude
import           FreeAgent.Orphans                  ()
import qualified Prelude                            as P

import Control.Monad.Reader (ReaderT, ask, mapReaderT, local)
import           Control.Monad.Writer               (Writer)
import           Data.Binary                        as Binary
import           Data.Dynamic                       (Dynamic)
import           Data.Time.Clock                    (UTCTime)
import           Data.Typeable                      (cast)

import           Control.DeepSeq                    (NFData (..))
import Control.DeepSeq.TH (deriveNFData)
import           FreeAgent.Process (MonadProcess(..), Process
                                                    ,ProcessId, NFSerializable
                                                    ,ChildSpec, RemoteTable
                                                    ,initRemoteTable, whereis
                                                    ,processNodeId)
import           Control.Distributed.Process.Node   (LocalNode)
import           Control.Distributed.Process (NodeId)
import           Control.Distributed.Process.Platform (Resolvable(..))
import           Control.Monad.Base                 (MonadBase)
import           Control.Monad.Logger               (LogLevel (..), LoggingT,
                                                     MonadLogger (..),
                                                     runStdoutLoggingT,
                                                     withChannelLogger)
import           Control.Monad.Trans.Control
import Control.Monad.State (StateT, mapStateT)
import Control.Error (mapEitherT)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value, Object)
import           Data.Default                       (Default (..))
import qualified Data.Set                           as Set
import           Data.UUID                          (UUID)
import           Data.SafeCopy (SafeCopy)



type Key = Text

-- | Types that can be serialized, stored and retrieved
--
class (SafeCopy a, Show a, Typeable a) => Stashable a where
    key :: a -> Key

-- Wrapped
-- | Wrapped lets us store an Action or Result and recover it using
-- it's registered Unwrapper
data Wrapped
  = Wrapped { wrappedKey        :: Key
            , wrappedTypeName   :: Text
            , wrappedValue      :: ByteString
            } deriving (Show, Eq, Typeable, Generic)

instance Stashable Wrapped where
    key = wrappedKey

{-instance FromJSON Wrapped where-}
    {-parseJSON (Object value') = do-}
        {-type'  <- value' .: "type"-}
        {-action' <- value' .: "action"-}
        {-return $ decodeJsonAction readPluginMaps type' action'-}
    {-parseJSON _ = mzero-}

{-instance ToJSON Wrapped where-}
    {-toJSON (Action action') =-}
        {-Aeson.object ["type" .= fqName action', "action" .= toJSON action']-}

-- ActionEnvelope
-- | A general type for transit of Actions through an agent network which
-- may not have the required plugins available to deserialize the
-- wrapped value.
data ActionEnvelope = ActionEnvelope
    { envelopeWrapped :: Wrapped
    , envelopeMeta    :: Object
    } deriving (Show, Eq, Typeable, Generic)

instance Stashable ActionEnvelope where
    key = wrappedKey . envelopeWrapped

data Action = forall a b. (Runnable a b) => Action a
        deriving Typeable

instance Eq Action where
    (Action a) == (Action b) = maybe False (a ==) (cast b)

instance Ord Action where
    (Action a) `compare` (Action b) = key a `compare` key b

instance P.Show Action where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance NFData Action where
    rnf (Action a) = rnf a

-- | Class for types that will result from some action and can
-- be boxed as 'Result'.
class ( NFSerializable result,  FromJSON result, ToJSON result
      , Stashable result, Extractable result)
    => Resulting result where
    -- | provide a 'ResultSummary'
    summary :: result -> ResultSummary
    -- | Create a generalized ResultMatcher function - see 'Core.resultMatcher'
    matchR :: (Typeable a) => (a -> Bool) -> result -> Bool

    matchR predicate result' = maybe False predicate (cast result')

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
type JsonUnwrapper a = (Value -> Either String a)
type Unwrapper a = (Wrapped -> Either String a)
type PluginConfigs = Map Text Dynamic

data ActionUnwrappers
  =  ActionUnwrappers { actionTypeName :: Text
                      , actionUnwrapper :: Unwrapper Action
                      , actionJsonUnwrapper :: JsonUnwrapper Action
                      , resultTypeName :: Text
                      , resultUnwrapper :: Unwrapper Result
                      , resultJsonUnwrapper :: JsonUnwrapper Result
                      }

type UnwrappersMap = Map Text ActionUnwrappers

type PluginWriter = Writer [PluginDef] ()
type ActionsWriter = Writer [ActionUnwrappers] ()
type ActionMatcher = (Action -> Bool)
type ResultMatcher = (Result -> Bool)

data Listener = ActionMatching ActionMatcher NodeId String
              | ResultMatching ActionMatcher ResultMatcher NodeId String
              deriving (Typeable)

-- | Dynamic wrapper around an open 'AcidState' and a function
-- which will close it - used with functions from
-- 'FreeAgent.Database.AcidState'


data ManagedResource =
    ManagedResource Dynamic (IO ())


data PluginDef
  = PluginDef { _plugindefName      :: !Text
              , _plugindefContext   :: !Dynamic
              , _plugindefActionUnwrappers :: ![ActionUnwrappers]
              , _plugindefListeners :: Agent [Listener]
              , _plugindefServers   :: [AgentServer]
              }

data PluginSet
  = PluginSet { _pluginsetUnwrappersMap   :: !UnwrappersMap
              , _pluginsetListeners       :: Agent [Listener]
              , _pluginsetConfigs         :: !PluginConfigs
              , _pluginsetPlugins         :: ![PluginDef]
              }

-- | AgentConfig data is set at startup and does not change while
-- the process is running
data AgentConfig
  = AgentConfig  { _configDbPath         :: !FilePath
                 , _configNodeHost       :: !String
                 , _configNodePort       :: !String
                 , _configDebugLogCount  :: !Int
                 , _configMinLogLevel    :: !LogLevel
                 , _configContexts       :: Set Context
                 , _configZones          :: Set Zone
                 , _configPeerNodeSeeds  :: [String]
                 , _configRemoteTable   :: !RemoteTable
                 }

instance Default AgentConfig where
    def = AgentConfig "./db" "127.0.0.1" "3546" 10 LevelWarn
            (Set.fromList [def])
            (Set.fromList [def])
            []
            initRemoteTable

-- | Each agent belongs to one or more contexts - every 'Package' specifies
-- the Context(s) in which it will execute
data Context = Context !Text deriving (Show, Eq, Ord, Typeable, Generic)

data Target =   Local
              | Remote Peer
              | Route [Context] [Zone]
              deriving (Show)

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
  = AgentContext { _contextAgentConfig     :: !AgentConfig
                 , _contextPlugins         :: !PluginSet
                 , _contextProcessNode     :: !LocalNode
                 , _contextOpenResources   :: MVar (Map Text ManagedResource)
                 , _contextTargetServer    :: Target -- ^ Target to which 'AgentServer' clients will send commands
                 }


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

-- | Typeclass for any monad stack based on 'Agent'
class ( Applicative agent, Monad agent, MonadIO agent, MonadBase IO agent, MonadBaseControl IO agent
      , ContextReader agent, MonadProcess agent, MonadLogger agent)
      => MonadAgent agent where
    withAgentContext :: (AgentContext -> AgentContext) -> agent a -> agent a

instance (MonadAgent agent)
      => MonadAgent (StateT a agent) where
    withAgentContext f = mapStateT (withAgentContext f)

instance MonadAgent agent => MonadAgent (EitherT fail agent) where
    withAgentContext f = mapEitherT (withAgentContext f)

newtype Agent a = Agent { unAgent :: ReaderT AgentContext (LoggingT Process) a}
            deriving ( Functor, Applicative, Monad, MonadBase IO
                     , ContextReader, MonadIO
                     )

instance MonadAgent Agent where
    withAgentContext f ma = Agent $ local f (unAgent ma)

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
                  | RSomeException Text
                  -- | Could not execute due to missing (local) dependency
                  -- (e.g. 'ruby' not found)
                  | DependencyFailure Text
                  -- | Could not execute due to unavailable network dependency
                  -- (e.g. could not reach database server)
                  | NetworkFailure Text
                  -- | Dependency response unexpected
                  | UnknownResponse Text
    deriving (Show, Eq, Typeable, Generic)

instance Convertible SomeException RunnableFail where
    safeConvert = return . RSomeException . tshow

data FailResult = FailResult RunnableFail ResultSummary
        deriving (Show,  Typeable, Generic)

-- | extract the concrete type under an Existential
class Typeable x => Extractable x where
    extract :: (Typeable a) => x -> Maybe a
    extract = cast

-- | Types which can be executed in the FreeAgent framework. Concrete
-- instances may be wrapped in the 'Action' existential type for compatibility
-- with all the basic plumbing implemented in FreeAgent Servers.
class ( NFSerializable action, NFSerializable result
      , Stashable action, Stashable result, Resulting result
      , Extractable action, Eq action
      , FromJSON action, FromJSON result, ToJSON action, ToJSON result)
     => Runnable action result | action -> result where
     -- | Perform the Action - implementing 'exec' is the minimum viable
     -- instance.
    exec :: (MonadAgent agent)
         => action -> agent (Either RunnableFail result)
    -- | Exec with some 'Result' - the default instance ignores
    -- the result and calls exec
    execWith :: (MonadAgent agent)
             =>  action -> Result -> agent (Either RunnableFail result)
    -- | Create a generalized ActionMatcher function - see 'Core.actionMatcher'
    matchA :: (Typeable a) => (a -> Bool) -> action -> Bool

    matchA predicate action' = maybe False predicate (cast action')
    execWith action' _ = exec action'

data ResultSummary
  = ResultSummary { _resultTimestamp :: UTCTime
                  , _resultText      :: Text
                  , _resultResultOf  :: Action
                  } deriving (Show, Eq, Typeable, Generic)

data ActionHistory = ActionHistory deriving (Show, Eq, Typeable, Generic)

data AgentServer = AgentServer { _serverName :: String
                               , _serverchildSpec :: AgentContext -> Process ChildSpec
                               }

instance Resolvable AgentServer where
    resolve (AgentServer sname _) = whereis sname

data ServerRef = ServerRef String ProcessId
                 deriving (Show, Eq, Generic)

instance Binary ServerRef

instance Ord ServerRef where
    ServerRef a _ `compare` ServerRef b _ = a `compare` b

data Peer = Peer { _peerUuid      :: UUID
                 , _peerProcessId :: !ProcessId
                 , _peerContexts  :: Set Context
                 , _peerZones     :: Set Zone
                 , _peerServers   :: Set ServerRef
                 } deriving (Show, Eq, Typeable, Generic)

instance Binary Peer

instance Ord Peer where
    a `compare` b = _peerUuid a `compare` _peerUuid b

instance Resolvable Peer where
    resolve = return . Just . _peerProcessId

instance Resolvable (Peer, String) where
    resolve (peer,sname) = resolve (nodeid, sname)
      where nodeid = processNodeId (_peerProcessId peer)


deriveSerializers ''Context
deriveSerializers ''Zone

deriveSafeStore ''Wrapped
deriveNFData ''Wrapped
instance Binary Wrapped

deriveSafeStore ''ActionEnvelope
deriveNFData ''ActionEnvelope
instance Binary ActionEnvelope

deriveSerializers ''RunnableFail

deriveNFData ''ServerRef
deriveNFData ''Peer
