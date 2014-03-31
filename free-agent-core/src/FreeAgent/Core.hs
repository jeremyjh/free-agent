{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}

module FreeAgent.Core
    ( runAgent
    , withAgent
    , spawnAgent
    , extractConfig
    , definePlugin
    , pluginSet
    , addPlugin
    , appendRemoteTable
    , thisNodeId
    , manageResource
    , lookupResource
    ) where

import           AgentPrelude
import           FreeAgent.Action                 (registerPluginMaps)
import           FreeAgent.Lenses
import           FreeAgent.Process
    ( spawnLocal, receiveWait, match
    , sendChan, SendPort, reregister
    , say, DiedReason(..), RemoteTable
    , localNodeId, NodeId)

import           Control.Monad.Reader             (runReaderT)
import           Control.Monad.Writer             (execWriter, tell)
import           Data.Dynamic                     (fromDynamic, toDyn )
import qualified Data.Map                         as Map

import           Control.Concurrent.Lifted        (threadDelay)
import           Control.Distributed.Process.Node (newLocalNode, runProcess)
import           Control.Distributed.Process.Management
import           Network.Transport                (closeTransport)
import           Network.Transport.TCP


import Control.Monad.Logger (runStdoutLoggingT)
-- | Execute the agent - main entry point
runAgent :: AgentConfig -> PluginSet -> Agent () -> IO ()
runAgent config' plugins' ma =
    catchAny
          ( do registerPluginMaps (plugins'^.unwrappersMap)
               statesMV <- newMVar mempty
               eithertcp <- createTransport (config'^.nodeHost)
                                            (config'^.nodePort)
                                            defaultTCPParameters
               (node, tcp) <- case eithertcp of
                   Right tcp -> do
                       node <- newLocalNode tcp $ config'^.remoteTable
                       return (node, tcp)
                   Left msg -> throwIO msg

               let context' = AgentContext
                                 { _contextAgentConfig = config'
                                 , _contextPlugins = plugins'
                                 , _contextProcessNode = node
                                 , _contextOpenResources = statesMV
                                 }

               let proc   = runStdoutLoggingT $ runReaderT (unAgent ma) context'

               runProcess node $ initLogger context' >> globalMonitor >> proc
               closeTransport tcp
               closeResources statesMV )

        ( \exception -> do
            putStrLn $ "Exception in runAgent: " ++ tshow exception
            return () )
  where
   -- overrides default Process logger to use Agent's logging framework
    initLogger context' = do
        pid <- spawnLocal logger
        reregister "logger" pid
        threadDelay 10000
     where
       logger =
         receiveWait
           [ match $ \((time, pid, string) ::(String, ProcessId, String)) -> do
               withAgent context' $ [qinfo|#{time} #{pid}: #{string} |]
               logger
           , match $ \((time, string) :: (String, String)) -> do
               -- this is a 'trace' message from the local node tracer
               withAgent context' $ [qdebug|#{time}: #{string} |]
               logger
           , match $ \(ch :: SendPort ()) -> -- a shutdown request
               sendChan ch ()
           ]

-- | Run an Agent action with an extracted AgentContext.
-- This is used to embed Agent code in the Process monad, for
-- example in ManagedProcess.
withAgent :: AgentContext -> Agent a -> Process a
withAgent ctxt ma =
    catchAny (runStdoutLoggingT $ runReaderT (unAgent ma) ctxt)
             $ \exception -> do
                 putStrLn $ "Exception in withAgent: " ++ tshow exception
                 throwIO exception

-- | Spawn a new process in the Agent monad.
spawnAgent :: AgentContext -> Agent a -> Process ProcessId
spawnAgent ctxt ma = spawnLocal (void $ withAgent ctxt ma)

-- | Lookup a plugin-specific config from the pluginConfigs map
-- and extract it to a concrete type.
extractConfig :: (ContextReader m, Typeable a) => Text -> m a
extractConfig configName = do
    configMap <- viewContext $ plugins.configs
    case Map.lookup configName configMap of
        Nothing ->
            error $ show $ configName ++ " not found! Did you register the plugin config?"
        Just dynconf ->
            maybe (error $ "fromDynamic failed for " ++ show configName)
                  return
                  (fromDynamic dynconf)

-- | Define the plugins to use in this program.
pluginSet :: PluginWriter -> PluginSet
pluginSet pluginWriter =
    let plugs = execWriter pluginWriter
        unwrappers = concatMap _plugindefActionUnwrappers plugs
        aconfigs = map buildConfigs plugs
        uwMap = buildPluginMaps unwrappers in

    PluginSet { _pluginsetUnwrappersMap = uwMap
              , _pluginsetListeners = buildListeners plugs
              , _pluginsetConfigs = Map.fromList aconfigs
              , _pluginsetPlugins = plugs
              }
  where
    buildConfigs plugin =
        (_plugindefName plugin, _plugindefContext plugin)
    buildPluginMaps unwrappers =
        let pairs = unzip $ map (\uw ->
                                    ( ("Action:" ++ actionTypeName uw, uw)
                                    , ("Result:" ++ resultTypeName uw, uw) ) )
                                unwrappers
            amap = Map.fromList (fst pairs)
            rmap = Map.fromList (snd pairs) in
        amap ++ rmap
    buildListeners = foldM appendListener []
    appendListener acc = _plugindefListeners >=> return . (++ acc)

-- | Add one particular plugin - call this in the 'PluginWriter'
-- opened by 'pluginSet'.
-- > addPlugin pd = tell [pd]
addPlugin :: PluginDef -> PluginWriter
addPlugin pd = tell [pd]

-- | used by Plugin *authors* to build PluginDef structures.
definePlugin :: (Typeable a)
             => Text -> a
             -> Agent [Listener]
             -> [AgentServer]
             -> ActionsWriter
             -> PluginDef
definePlugin pname pcontext listeners' servers' pwriter
  = PluginDef pname (toDyn pcontext) (execWriter pwriter) listeners' servers'

globalMonitor :: Process ()
globalMonitor = do
    let initState = [] :: [MxEvent]
    void $ mxAgent (MxAgentId "lifecycle-listener-agent") initState [
        mxSink $ \ev -> do
           let act =
                 case ev of
                 -- TODO: need to promote ERROR logs so they come out in
                 -- LevelError
                   (MxProcessDied pid (DiedException msg)) -> liftMX $ say $ debug $ "[Error] " ++ show pid ++  " DiedException: " ++ msg
                   (MxProcessDied pid DiedDisconnect) -> liftMX $ say $ show pid ++ " DiedDisconnect"
                   (MxNodeDied nodeId (DiedException msg)) -> liftMX $ say $ debug $ "[Error] " ++ show nodeId ++  " DiedException: " ++ msg
                   (MxNodeDied nodeId DiedDisconnect) -> liftMX $ say $ show nodeId ++ " DiedDisconnect"
                   _                   -> return ()
           act >> mxReady ]
    threadDelay 10000

-- | Add a module's __remotetable to the RemoteTable that will
-- be used to activate the node
appendRemoteTable :: (RemoteTable -> RemoteTable) -> AgentConfig -> AgentConfig
appendRemoteTable table config' = config' & remoteTable %~ table

-- | NodeId for this Agent
thisNodeId :: (ContextReader m) => m NodeId
thisNodeId = viewContext $ processNode.to localNodeId

-- | Register a named, managed resource. A managed resource will be closed
-- when the Agent shuts down by invoking the () -> IO () parameter.
-- Note that the resource is "namespaced" with its type. Registering
-- two different typed values with the same name is fine. Registering a
-- second instance of a type with the same name will throw an exception.
manageResource :: (MonadAgent m, Typeable a)
            => Text -> a ->  IO () -> m ()
manageResource name' resource' closer = do
    resourcesMV <- viewContext openResources
    modifyMVar_ resourcesMV insertIf
  where insertIf resources
           | Map.member name' resources =
                throwIO $ userError $ "Resource: " ++ convert name' ++ " is already managed!"
           | otherwise =
                return $ Map.insert qualifiedName
                                    (ManagedResource (toDyn resource') closer)
                                    resources
        qualifiedName = fqName resource' ++ name'

-- | Find and return a resource value by name if is registered.
lookupResource :: forall a m. (MonadAgent m, Typeable a)
               => Text -> m (Maybe a)
lookupResource name' = do
    mvstates <- viewContext openResources
    states <- readMVar mvstates
    return $ maybe Nothing
                   fromDynamic
                   (value <$> Map.lookup qualifiedName states)
  where value (ManagedResource v _) = v
        qualifiedName = fqName (undefined::a) ++ name'

closeResources :: (MonadBase IO m, MonadIO m)
               => MVar (Map Text ManagedResource) -> m ()
closeResources statesMV = do
    handles <-  takeMVar statesMV
    forM_ handles $ \ (ManagedResource _ closeFn) ->
        liftIO $ closeFn
