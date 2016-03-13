{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses            #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies            #-}
{-# LANGUAGE BangPatterns, UndecidableInstances #-}


module FreeAgent.Core.Internal.Types
 ( module FreeAgent.Core.Internal.Types
 , MonadProcess(..)
 , NFSerializable, Process, ProcessId
 , MonadBase
 , MonadBaseControl
 , NFData(..)
 , LogLevel(..), MonadLogger
 , UUID
 , FromJSON(..), ToJSON (..)
 )

where

import           FreeAgent.AgentPrelude
import           FreeAgent.Orphans                    ()
import qualified Prelude                              as P

import           Control.Concurrent                   (MVar)
import           Control.Monad.Reader                 (ReaderT, ask, local)
import           Control.Monad.Writer                 (Writer)
import           Data.Binary                          as Binary
import           Data.Dynamic                         (Dynamic)
import           Data.Typeable                        (cast)

import           Control.Distributed.Process.Lifted.Class
import           Control.Distributed.Process          (NodeId)
import           Control.Distributed.Process.Node     (LocalNode)
import           Control.Distributed.Process.Extras   (Resolvable (..))
import           Control.Error                        (mapExceptT)
import           Control.Monad.Base                   (MonadBase)
import           Control.Monad.Logger                 (LogLevel (..), LoggingT,
                                                       MonadLogger (..),
                                                       runStdoutLoggingT,
                                                       withChannelLogger)
import           Control.Monad.State                  (StateT, mapStateT)
import           Control.Monad.Trans.Control
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..), Value (..),
                                                       object, (.:), (.:?), (.=))
import qualified Data.ByteString.Base64               as B64
import           Data.Default                         (Default (..))
import           Data.SafeCopy                        --(SafeCopy(..))
import qualified Data.Serialize                       as Cereal
import qualified Data.Set                             as Set
import           Data.UUID                            (UUID)
import           FreeAgent.Process                    (ChildSpec,
                                                       NFSerializable, Process,
                                                       ProcessId, RemoteTable,
                                                       initRemoteTable,
                                                       processNodeId, whereis)



type Key = Text

-- | Types that can be serialized, stored and retrieved
--
class (SafeCopy a, Show a, Typeable a) => Stashable a where
    key :: a -> Key

class (Stashable a, Eq a, ToJSON a, FromJSON a) => Portable a where

instance (Stashable a, Eq a, ToJSON a, FromJSON a) => Portable a

-- | Wrapped lets us store an Action or Result and recover it using
         -- it's registered Unwrapper
data Wrapped
  = WrappedEncoded  !Key !Text !ByteString
  | WrappedJson !Key !Text !Value
  | forall a. Portable a =>
    WrappedExists !Text !a
        deriving (Typeable)

wrappedTypeName :: Wrapped -> Text
wrappedTypeName (WrappedEncoded _ type' _) = type'
wrappedTypeName (WrappedJson  _ type' _) = type'
wrappedTypeName (WrappedExists type' _) = type'

instance NFData Wrapped where
    rnf = (`seq` ())

instance Show Wrapped where
    show (WrappedEncoded key' type' _) = "WrappedEncoded: " ++ show (key', type')
    show (WrappedJson key' type' val') = "WrappedJson: " ++ show (key', type',val')
    show (WrappedExists _ payload) = "WrappedExists: " ++ show payload

safeEncode :: SafeCopy a => a -> ByteString
safeEncode = Cereal.runPut . safePut

instance Eq Wrapped where
  (WrappedEncoded key' type' bytes') == (WrappedEncoded key'' type'' bytes'') =
      key' == key'' && type' == type'' && bytes' == bytes''
  (WrappedEncoded key' type' bytes') == (WrappedExists type'' payload) =
      key' == key payload && type' == type'' && bytes' == safeEncode payload
  (WrappedJson key' type' val') == (WrappedJson key'' type'' val'') =
      key' == key'' && type' == type'' && val' == val''
  (WrappedJson key' type' val') == (WrappedExists type'' payload) =
      key' == key payload && type' == type'' && val' == toJSON payload
  (WrappedExists type' a) == (WrappedExists type'' b) =
      type' == type'' && maybe False (a ==) (cast b)
  w1@WrappedExists {} == w2@WrappedEncoded {} = w2 == w1
  w1@WrappedExists {} == w2@WrappedJson {} = w2 == w1
  WrappedEncoded {} == WrappedJson {} = False
  WrappedJson {} == WrappedEncoded {} = False

instance SafeCopy Wrapped where
    version = 1
    kind = base
    errorTypeName _ = "FreeAgent.Core.Internal.Types.Wrapped"

    putCopy (WrappedEncoded key' type' bytes') = contain $
     do Cereal.putWord8 0
        safePut key'
        safePut type'
        safePut bytes'
    putCopy (WrappedJson key' type' val') = contain $
     do Cereal.putWord8 1
        safePut key'
        safePut type'
        safePut val'
    putCopy (WrappedExists type' payload) =
        putCopy (WrappedEncoded (key payload)
                                type'
                                (safeEncode payload))

    getCopy = contain $
      do tag <- Cereal.getWord8
         case tag of
             0 -> WrappedEncoded <$> safeGet <*> safeGet <*> safeGet
             1 -> WrappedJson <$> safeGet <*> safeGet <*> safeGet
             _ -> fail "Unidentified tag when deseralizing FreeAgent.Wrapped."

instance Binary Wrapped where
    get =
      do bs <- Binary.get
         let ewrapped = Cereal.runGet safeGet bs
         case ewrapped of
             Right wrapped -> return wrapped
             Left msg -> error $ "Error deserializing Wrapped: " ++ msg

    put = Binary.put . safeEncode

instance Stashable Wrapped where
  key (WrappedEncoded key' _ _)= key'
  key (WrappedJson key' _ _)= key'
  key (WrappedExists _ payload)= key payload

instance FromJSON Wrapped where
    parseJSON (Object value') = do
        key' <- value' .: "key"
        typeName' <- value' .: "typeName"
        mb64T :: Maybe Text <- value' .:? "bytes"
        case mb64T of
            Just b64T ->
                return (WrappedEncoded key' typeName' (B64.decodeLenient (convert b64T)))
            Nothing -> do
                val <- value' .: "value"
                return (WrappedJson key' typeName' val) 
    parseJSON _ = mzero

instance ToJSON Wrapped where
    toJSON (WrappedEncoded key' typeName' value') =
        let b64T = (convert $ B64.encode value') :: Text
        in object ["key" .= key', "typeName" .= typeName', "bytes" .= b64T]
    toJSON (WrappedJson key' typeName' value') =
        object ["key" .= key', "typeName" .= typeName', "value" .= value']
    toJSON (WrappedExists type' payload) =
        object ["key" .= key payload, "typeName" .= type', "value" .=  toJSON payload]

data Action = forall a. (Runnable a) => Action a
        deriving Typeable

instance Eq Action where
    (Action a) == (Action b) = maybe False (a ==) (cast b)

instance Ord Action where
    (Action a) `compare` (Action b) = key a `compare` key b

instance P.Show Action where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance NFData Action where
    rnf (Action a) = rnf a

-- Result
-- | Box for returning results from 'Action' exec.
--

type FetchAction = Either String Action
-- Unwrapper and registration types
type JsonUnwrapper a = (Value -> Either String a)
type Unwrapper a = (Wrapped -> Either String a)
type PluginConfigs = Map Text Dynamic

data ActionUnwrappers
  =  ActionUnwrappers { actionTypeName      :: Text
                      , actionUnwrapper     :: Unwrapper Action
                      }

data ResultUnwrappers
  =  ResultUnwrappers { resultTypeName      :: Text
                                               --TODO: why is this not :: Unwrapper Result
                      , resultUnwrapper     :: Result -> Result
                      }

type PluginWriter = Writer [PluginDef] ()
type ActionsWriter = Writer [(ActionUnwrappers, ResultUnwrappers)] ()
type ActionMatcher = (Action -> Bool)
type ResultMatcher = (Result -> Bool)

data Listener = ActionMatching ActionMatcher NodeId String
              | ResultMatching ActionMatcher ResultMatcher NodeId String
              deriving (Typeable)

-- | Dynamic wrapper around an open resource (e.g. 'AcidState')
-- and a function which will close it
data ManagedResource =
    ManagedResource Dynamic (IO ())

data PluginDef
  = PluginDef { plugindefName             :: !Text
              , plugindefContext          :: !Dynamic
              , plugindefActionUnwrappers :: ![ActionUnwrappers]
              , plugindefResultUnwrappers :: ![ResultUnwrappers]
              , plugindefListeners        :: Agent [Listener]
              , plugindefServers          :: [AgentServer]
              }

data PluginSet
  = PluginSet { pluginsetActionUnwrappers :: !(Map Text ActionUnwrappers)
              , pluginsetResultUnwrappers :: !(Map Text ResultUnwrappers)
              , pluginsetListeners     :: Agent [Listener]
              , pluginsetConfigs       :: !PluginConfigs
              , pluginsetPlugins       :: ![PluginDef]
              }

-- | AgentConfig data is set at startup and does not change while
-- the process is running
data AgentConfig
  = AgentConfig  { configDbPath        :: !FilePath
                 , configNodeHost      :: !String
                 , configNodePort      :: !String
                 , configDebugLogCount :: !Int
                 , configMinLogLevel   :: !LogLevel
                 , configContexts      :: Set Context
                 , configZones         :: Set Zone
                 , configPeerNodeSeeds :: [String]
                 , configRemoteTable   :: !RemoteTable
                 , configInitScheduler :: !Bool
                 }

instance Default AgentConfig where
    def = AgentConfig "./db" "127.0.0.1" "3546" 10 LevelWarn
            (Set.fromList [def])
            (Set.fromList [def])
            []
            initRemoteTable
            True

-- | Each agent belongs to one or more contexts - every 'Package' specifies
-- the Context(s) in which it will execute
data Context = Context !Text deriving (Show, Eq, Ord, Typeable, Generic)

data Target =   Local
                -- ^ the local Process dictionary will register names looked
                -- up on the remote nodes - the remote is specified as
                -- "host:port"
              | RemoteCache String
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
  = AgentContext { contextAgentConfig   :: !AgentConfig
                 , contextPlugins       :: !PluginSet
                 , contextProcessNode   :: !LocalNode
                 , contextOpenResources :: MVar (Map Text ManagedResource)
                 , contextTargetServer  :: Target -- ^ Target to which 'AgentServer' clients will send commands
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

instance ContextReader m => ContextReader (ExceptT e m) where
    askContext = lift askContext

-- | Typeclass for any monad stack based on 'Agent'
class ( Applicative agent, Monad agent, MonadIO agent, MonadBase IO agent, MonadBaseControl IO agent
      , ContextReader agent, MonadProcess agent, MonadLogger agent)
      => MonadAgent agent where
    withAgentContext :: (AgentContext -> AgentContext) -> agent a -> agent a

instance (MonadAgent agent)
      => MonadAgent (StateT a agent) where
    withAgentContext f = mapStateT (withAgentContext f)

instance MonadAgent agent => MonadAgent (ExceptT fail agent) where
    withAgentContext f = mapExceptT (withAgentContext f)

newtype Agent a = Agent { unAgent :: ReaderT AgentContext Process a}
            deriving ( Functor, Applicative, Monad, MonadBase IO
                     , ContextReader, MonadIO
                     )

instance MonadAgent Agent where
    withAgentContext f ma = Agent $ local f (unAgent ma)

instance MonadBaseControl IO Agent where
  type StM Agent a = StM (ReaderT AgentContext Process) a
  liftBaseWith f = Agent $ liftBaseWith $ \ rib -> f (rib . unAgent)
  restoreM = Agent . restoreM

instance MonadProcessBase Agent where
  type StMP Agent a = StMP (ReaderT AgentContext Process) a
  restoreMP = Agent . restoreMP
  liftBaseWithP f = Agent $ liftBaseWithP $ \ rib -> f (rib . unAgent)

instance MonadLogger Agent where
    monadLoggerLog !a !b !level !d =
        doLog =<< configMinLogLevel . contextAgentConfig <$> askContext
      where doLog minlev
              | level >= minlev = runStdoutLoggingT $ monadLoggerLog a b level d
              | otherwise = a `seq` b `seq` d `seq` return ()

runAgentLoggingT :: (MonadIO m, MonadBaseControl IO m) => Int -> LoggingT m a -> m a
runAgentLoggingT debugCount = runStdoutLoggingT . withChannelLogger debugCount

instance MonadProcess Agent where
    liftP ma = Agent $ lift ma

-- | Failure modes for exec method of Runnable
-- A failure should mean that the action could not
-- complete execution - negative results of successful execution
-- should be captured in the Resulting type
data RunnableFail =
                  -- | Action-specific general failure to exec
                     GeneralFailure Text
                  -- | Deserialization failure - plugin unregistered?
                  | DeserializationFailure Text
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

data FailResult = FailResult RunnableFail Key
        deriving (Show, Eq,  Typeable, Generic)

-- | Types which can be executed in the FreeAgent framework. Concrete
-- instances may be wrapped in the 'Action' existential type for compatibility
-- with all the basic plumbing implemented in FreeAgent Servers.
class ( NFSerializable action
      , Eq action
      , Portable action
      , Portable (RunnableResult action)
      )
     => Runnable action where

    type RunnableResult action
    type RunnableResult action = Result

     -- | Perform the Action - implementing 'exec' is the minimum viable
     -- instance.
    exec :: (MonadAgent agent)
         => action -> agent (Either RunnableFail Result)
    -- | Exec with some 'Result' - the default instance ignores
    -- the result and calls exec
    execWith :: (MonadAgent agent)
             =>  action -> Result -> agent (Either RunnableFail Result)

    execWith action' _ = exec action'

data Result
  = Result        { resultWrapped   :: Wrapped
                  , resultTimestamp :: UTCTime
                  , resultText      :: Text
                  , resultResultOf  :: Action
                  } deriving (Show, Eq, Typeable, Generic)

data ActionHistory = ActionHistory deriving (Show, Eq, Typeable, Generic)

data AgentServer = AgentServer { aserverName      :: String
                               , aserverChildSpec :: AgentContext -> Process ChildSpec
                               }

instance Resolvable AgentServer where
    resolve (AgentServer sname _) = whereis sname

data ServerRef = ServerRef String ProcessId
               | PartialRef String -- ^ use only for Set filters
                 deriving (Show, Eq, Generic, Typeable)

instance Binary ServerRef

instance Ord ServerRef where
    ServerRef a _ `compare` ServerRef b _ = a `compare` b
    ServerRef a _ `compare` PartialRef b  = a `compare` b
    PartialRef a  `compare` PartialRef b  = a `compare` b
    PartialRef a  `compare` ServerRef b _  = a `compare` b

data Peer = Peer { peerUuid      :: UUID
                 , peerProcessId :: !ProcessId
                 , peerContexts  :: Set Context
                 , peerZones     :: Set Zone
                 , peerServers   :: Set ServerRef
                 } deriving (Show, Eq, Typeable, Generic)

instance Binary Peer

instance Ord Peer where
    a `compare` b = peerUuid a `compare` peerUuid b

instance Resolvable Peer where
    resolve = return . Just . peerProcessId

instance Resolvable (Peer, String) where
    resolve (peer,sname) = resolve (nodeid, sname)
      where nodeid = processNodeId (peerProcessId peer)


deriveSerializers ''Context
deriveSerializers ''Zone

deriveSerializers ''RunnableFail

instance NFData ServerRef where rnf = genericRnf
instance NFData Peer where rnf = genericRnf
