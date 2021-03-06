{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, OverloadedStrings, PatternGuards #-}
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables                                      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- Default PluginSet

module FreeAgent.Core.Agent
    ( runAgent
    , withAgent
    , forkAgent
    , spawnAgent
    , extractConfig
    , definePlugin
    , pluginSet
    , addPlugin
    , appendRemoteTable
    , thisNodeId
    , manageResource
    , lookupResource
    , withTarget
    , withRemoteNode
    , agentAsync
    ) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Action                  (actionType, registerAction)
import           FreeAgent.Core.Action.Composition      (ActionPlan, agentAsync,
                                                         withAgent)
import           FreeAgent.Core.Action.ShellCommand     (ShellCommand)
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Core.Protocol.Peer           (warmRemoteCache)
import           FreeAgent.Process                      (DiedReason (..), NodeId,
                                                         RemoteTable, SendPort,
                                                         localNodeId, match, receiveWait,
                                                         reregister, say, sendChan,
                                                         spawnLocal)
import qualified Prelude                                as P

import           Control.Monad.Reader                   (runReaderT)
import           Control.Monad.Writer                   (execWriter, tell)
import           Data.Default                           (Default)
import           Data.Dynamic                           (fromDynamic, toDyn)
import qualified Data.Map                               as Map

import           Control.Concurrent.Lifted              (threadDelay)
import           Control.Concurrent.MVar.Lifted
import           Control.Distributed.Process.Management
import           Control.Distributed.Process.Node       (forkProcess, newLocalNode,
                                                         runProcess)
import           Network.Transport                      (closeTransport)
import           Network.Transport.TCP




-- need this here so we include Core.pluginDef by default
instance Default PluginSet where
    def = pluginSet (return ())

-- | Execute the agent - main entry point
runAgent :: AgentConfig -> PluginSet -> Agent () -> IO ()
runAgent config' plugins' ma =
    catchAny
        ( do statesMV <- newMVar mempty
             bracket openTransport
                     (closeResources statesMV)
                     (\ tcp ->
                        do node <- newLocalNode tcp (config' ^. remoteTable)
                           let context' = AgentContext
                                              { contextAgentConfig = config'
                                              , contextPlugins = plugins'
                                              , contextProcessNode = node
                                              , contextOpenResources = statesMV
                                              , contextTargetServer = Local
                                              }
                           let proc = runReaderT (unAgent ma) context'

                           runProcess node $ do
                                        initLogger context'
                                        globalMonitor
                                        proc
                     )
        )
        (\ exception -> do
            putStrLn $ "Exception in runAgent: " ++ tshow exception
            return ()
        )
  where
    openTransport = do
        etcp <- createTransport (config' ^. nodeHost) (config' ^. nodePort) defaultTCPParameters
        case etcp of
            Right tcp -> return tcp
            Left msg -> throwIO msg
    closeResources statesMV tcp = do
        closeTransport tcp
        handles <-  takeMVar statesMV
        forM_ handles $ \ (ManagedResource _ closeFn) ->
            liftIO closeFn
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

-- | 'forkProcess' analogue for the Agent monad
forkAgent :: AgentContext -> Agent () -> IO ProcessId
forkAgent context' =
    forkProcess (context' ^. processNode) . withAgent context'

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
    let plugs = execWriter pluginWriter <> [pluginDef]
        aunwrappers = concatMap plugindefActionUnwrappers plugs
        runwrappers = concatMap plugindefResultUnwrappers plugs
        aconfigs = map buildConfigs plugs
    in
    PluginSet { pluginsetActionUnwrappers = buildActionPluginMaps aunwrappers
              , pluginsetResultUnwrappers = buildResultPluginMaps runwrappers
              , pluginsetListeners = buildListeners plugs
              , pluginsetConfigs = Map.fromList aconfigs
              , pluginsetPlugins = plugs
              }
  where
    buildConfigs plugin =
        (plugindefName plugin, plugindefContext plugin)
    buildActionPluginMaps uws =
        let pairs = map (\uw -> (actionTypeName uw, uw)) uws
        in Map.fromList pairs
    buildResultPluginMaps uws =
         let pairs = map (\uw -> (resultTypeName uw, uw)) uws
         in Map.fromList pairs
    buildListeners = foldM appendListener []
    appendListener acc = plugindefListeners >=> return . (++ acc)

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
  = PluginDef pname (toDyn pcontext) (pick fst) (pick snd) listeners' servers'
  where pick f = map f (execWriter pwriter)

globalMonitor :: Process ()
globalMonitor = do
    let initState = [] :: [MxEvent]
    void $ mxAgent (MxAgentId "lifecycle-listener-agent") initState [
        mxSink $ \ev -> do
           let act =
                 case ev of
                 -- TODO: need to promote ERROR logs so they come out in
                 -- LevelError
                   {-(MxSpawned pid) -> print pid-}
                   (MxRegistered pid name') -> liftMX $ say ("Registered " ++ show pid ++ " as: " ++ name' )
                   (MxProcessDied pid (DiedException msg)) ->
                    let sayException = "[Error] " ++ show pid ++  " DiedException: " ++ msg
                    in liftMX $ say $ case P.words msg of
                       "ProcessLinkException" : _                -> sayException
                       reason : _ | "exit-from" <- take 9 reason -> sayException
                                  | "reason=testing" <- reverse $ take 14 (reverse reason)
                                                                 -> sayException
                                  | otherwise                    -> debug sayException
                       _ -> debug sayException
                   (MxProcessDied pid DiedDisconnect) ->
                       liftMX $ say $ show pid ++ " DiedDisconnect"
                   (MxNodeDied nodeId (DiedException msg)) ->
                       liftMX $ say $ debug $ "[Error] " ++ show nodeId ++  " DiedException: " ++ msg
                   (MxNodeDied nodeId DiedDisconnect) ->
                       liftMX $ say $ show nodeId ++ " DiedDisconnect"
                   _                   -> return ()
           act >> mxReady ]
    threadDelay 10000

-- | Add a module's __remotetable to the RemoteTable that will
-- be used to activate the node
appendRemoteTable :: (RemoteTable -> RemoteTable) -> AgentConfig -> AgentConfig
appendRemoteTable table = remoteTable %~ table

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
        qualifiedName = proxyFqName (Proxy :: Proxy a) ++ name'

-- | Register the Core Actions
pluginDef :: PluginDef
pluginDef = definePlugin "Core" () (return []) [] $
 do registerAction (actionType :: Proxy ShellCommand)
    registerAction (actionType :: Proxy ActionPlan)

-- | Connect to a remote node specified by "host:port" and execute
-- an Agent computation with that node as the target.
withRemoteNode :: MonadAgent agent => String -> agent a -> agent a
withRemoteNode nodestr ma =
    withTarget (RemoteCache nodestr) $ warmRemoteCache nodestr >> ma

withTarget :: MonadAgent agent => Target -> agent a -> agent a
withTarget target =
    withAgentContext (targetServer .~ target)
