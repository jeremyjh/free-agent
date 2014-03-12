{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module FreeAgent.Core
    ( runAgent
    , withAgent
    , spawnAgent
    , extractConfig
    , definePlugin
    , registerPlugins
    , addPlugin
    , appendRemoteTable
    , thisNodeId
    ) where

import           AgentPrelude
import           FreeAgent.Action                 (registerPluginMaps)
import           FreeAgent.Database
import           FreeAgent.Database.AcidState (closeAllStates)
import           FreeAgent.Lenses
import           FreeAgent.Process
    ( spawnLocal, receiveWait, match
    , sendChan, SendPort, reregister
    , say, DiedReason(..), RemoteTable
    , localNodeId, NodeId)

import           Control.Exception
import           Control.Monad.Reader             (runReaderT)
import           Control.Monad.Writer             (execWriter, tell)
import           Data.Dynamic                     (fromDynamic, toDyn)
import qualified Data.Map                         as Map

import           Control.Concurrent.Lifted        (threadDelay)
import           Control.Distributed.Process.Node (newLocalNode, runProcess)
import           Control.Distributed.Process.Management
import           Network.Transport                (closeTransport)
import           Network.Transport.TCP


import Control.Monad.Logger (runStdoutLoggingT)
-- | Execute the agent - main entry point
runAgent :: AgentContext -> Agent () -> IO ()
runAgent ctxt ma =
    catchAny
          ( do registerPluginMaps (ctxt^.actionMap, ctxt^.resultMap)
               (_, dbChan) <- initAgentDB ctxt
               statesMV <- newMVar mempty
               eithertcp <- createTransport (ctxt^.agentConfig.nodeHost)
                                            (ctxt^.agentConfig.nodePort)
                                            defaultTCPParameters
               (node, tcp) <- case eithertcp of
                   Right tcp -> do
                       node <- newLocalNode tcp $ ctxt^.remoteTable
                       return (node, tcp)
                   Left msg -> throw msg

               let ctxt'  = ctxt & agentDBChan .~ dbChan
                                 & processNode .~ node
                                 & openStates .~ statesMV
               let proc   = runStdoutLoggingT $ runReaderT (unAgent ma) ctxt'

               {-runProcess node $ initLogger >> proc-}
               runProcess node $ initLogger >> globalMonitor >> proc
               closeTransport tcp
               closeAgentDB dbChan
               closeAllStates statesMV )

        ( \exception -> do
            putStrLn $ "Exception in runAgent: " ++ tshow exception
            return () )
  where
   -- overrides default Process logger to use Agent's logging framework
    initLogger = do
        pid <- spawnLocal logger
        reregister "logger" pid
        threadDelay 10000
     where
       logger =
         receiveWait
           [ match $ \((time, pid, string) ::(String, ProcessId, String)) -> do
               withAgent ctxt $ [qinfo|#{time} #{pid}: #{string} |]
               logger
           , match $ \((time, string) :: (String, String)) -> do
               -- this is a 'trace' message from the local node tracer
               withAgent ctxt $ [qdebug|#{time}: #{string} |]
               logger
           , match $ \(ch :: SendPort ()) -> -- a shutdown request
               sendChan ch ()
           ]

-- | Used to embed Agent code in the Process monad
withAgent :: AgentContext -> Agent a -> Process a
withAgent ctxt ma =
    catchAny (runStdoutLoggingT $ runReaderT (unAgent ma) ctxt)
             $ \exception -> do
                 putStrLn $ "Exception in withAgent: " ++ tshow exception
                 throw exception

-- | Spawn a new process with an Agent Context and throwing
-- away the result
spawnAgent :: AgentContext -> Agent a -> Process ProcessId
spawnAgent ctxt ma = spawnLocal (void $ withAgent ctxt ma)

-- | Lookup a plugin-specific config from the pluginConfigs map
-- and extract it to a concrete type with fromDynamic
extractConfig :: (ContextReader m, Typeable a) => ByteString -> m a
extractConfig configName = do
    configMap <- viewConfig $ agentConfig.pluginContexts
    case Map.lookup configName configMap of
        Nothing ->
            error $ show $ configName ++ " not found! Did you register the plugin config?"
        Just dynconf ->
            maybe (error "fromDynamic failed for NagiosConfig!")
                  return
                  (fromDynamic dynconf)

registerPlugins :: PluginWriter -> AgentContext
registerPlugins pw =
    let plugs = execWriter pw
        acts = concatMap _plugindefActions plugs
        acontexts = map buildContexts plugs
        (amap, rmap) = buildPluginMaps acts in
    def { _contextActionMap = amap
        , _contextResultMap = rmap
        , _contextAgentConfig = def {_configPluginContexts = Map.fromList acontexts}
        , _contextListeners = buildListeners plugs
        }
  where
    buildContexts plugin =
        (_plugindefName plugin, _plugindefContext plugin)
    buildPluginMaps plugs =
        let pairs = unzip $ map (\(q1, q2, q3, q4) -> ((q1, q2), (q3, q4))) plugs
            amap = Map.fromList (fst pairs)
            rmap = Map.fromList (snd pairs) in
        (amap, rmap)
    buildListeners = foldM appendListener []
    appendListener acc = _plugindefListeners >=> return . (++ acc)

addPlugin :: PluginDef -> PluginWriter
addPlugin pd = tell [pd]

definePlugin :: (Typeable a)
             => ByteString -> a
             -> Agent [Listener]
             -> ActionsWriter
             -> PluginDef
definePlugin pname pcontext listeners' pwriter
  = PluginDef pname (toDyn pcontext) (execWriter pwriter) listeners'

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
appendRemoteTable :: (RemoteTable -> RemoteTable) -> AgentContext -> AgentContext
appendRemoteTable table ctxt =
    ctxt & remoteTable .~ table (ctxt^.remoteTable)

thisNodeId :: (ContextReader m) => m NodeId
thisNodeId = viewConfig $ processNode.to localNodeId
