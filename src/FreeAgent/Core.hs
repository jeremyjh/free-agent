{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module FreeAgent.Core
    ( runAgent
    , withAgent
    , spawnAgent
    , extractConfig
    , definePlugin
    , registerPlugins
    , addPlugin
    ) where

import           FreeAgent.Action                 (registerPluginMaps)
import           FreeAgent.Database
import           FreeAgent.Lenses
import {-# SOURCE #-} FreeAgent.Peer (peerServer, execServer)
import           AgentPrelude

import           Control.Exception
import           Control.Monad.Reader             (runReaderT)
import           Control.Monad.Writer             (execWriter, tell)
import           Data.Dynamic                     (fromDynamic, toDyn)
import qualified Data.Map                         as Map

import           FreeAgent.Process (spawnLocal)
import           Control.Distributed.Process.Node ( newLocalNode, runProcess
                                                  , initRemoteTable )
import           Control.Distributed.Process.Platform.Supervisor
import Control.Concurrent.Lifted (threadDelay)
import           Network.Transport                (closeTransport)
import           Network.Transport.TCP




-- | Execute the agent - main entry point
runAgent :: AgentContext -> Agent () -> IO ()
runAgent ctxt ma = do
    registerPluginMaps (ctxt^.actionMap, ctxt^.resultMap)
    (_, dbChan) <- initAgentDB ctxt
    eithertcp <- createTransport (ctxt^.agentConfig.nodeHost)
                                 (ctxt^.agentConfig.nodePort)
                                 defaultTCPParameters
    (node, tcp) <- case eithertcp of
        Right tcp -> do
            node <- newLocalNode tcp initRemoteTable
            return (node, tcp)
        Left msg -> throw msg

    let ctxt'  = ctxt & agentDBChan .~ dbChan
                      & processNode .~ node

    let proc   = runAgentLoggingT (ctxt^.agentConfig.debugLogCount) $
                    runReaderT (unAgent ma) $ ctxt'

    runProcess node $ (startSuper ctxt') >> proc
    closeTransport tcp
    closeAgentDB dbChan
  where
    startSuper ctxt' = do
        cspecs <- sequence $ fmap childFrom [peerServer, execServer]
        void $ start restartOne cspecs
        threadDelay 10000
      where childFrom (AgentServer _ _ child) = child ctxt'

-- | Used to embed Agent code in the Process monad
withAgent :: AgentContext -> Agent a -> Process a
withAgent ctxt ma =
    runAgentLoggingT (ctxt^.agentConfig.debugLogCount) $
        runReaderT (unAgent ma) ctxt

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
            case fromDynamic dynconf of
                Nothing -> error "fromDynamic failed for NagiosConfig!"
                Just nagconf -> return nagconf

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
  = PluginDef { _plugindefName = pname
              , _plugindefContext = toDyn pcontext
              , _plugindefActions = execWriter pwriter
              , _plugindefListeners = listeners'
              }
