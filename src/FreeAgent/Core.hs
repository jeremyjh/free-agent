{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Core
    ( runAgent
    , viewConfig
    , extractConfig
    , definePlugin
    , registerPlugins
    , addPlugin
    ) where

import           FreeAgent.Prelude
import           FreeAgent.Lenses
import           FreeAgent.Database
import           FreeAgent.Action                  (registerPluginMaps)

import           Control.Monad.Writer              (execWriter, tell)
import           Control.Monad.Reader              (runReaderT)
import           Control.Exception
import           Control.Distributed.Process.Node
import           Network.Transport.TCP
import           Network.Transport (closeTransport)

import qualified Data.Map as Map
import           Data.Dynamic (toDyn, fromDynamic)

-- | Execute the agent - main entry point
runAgent :: AgentContext -> Agent () -> IO ()
runAgent ctxt ma = do
    registerPluginMaps (ctxt^.actionMap, ctxt^.resultMap)
    (_, dbChan) <- initAgentDB ctxt
    let ctxt' = ctxt & agentDBChan .~ dbChan
    let proc = runReaderT (unAgent ma) ctxt'
    eithertcp <- createTransport (ctxt^.agentConfig.nodeHost)
                                 (ctxt^.agentConfig.nodePort)
                                 defaultTCPParameters
    case eithertcp of
        Right tcp -> do
            node <- newLocalNode tcp initRemoteTable
            runProcess node proc
            closeTransport tcp
        Left msg -> throw msg
    closeAgentDB dbChan

-- | Use a lens to view a portion of AgentContext
viewConfig :: (ConfigReader m) => Getting a AgentContext a -> m a
viewConfig lens = do
    conf <- askConfig
    return $ view lens conf

-- | Lookup a plugin-specific config from the pluginConfigs map
-- and extract it to a concrete type with fromDynamic
extractConfig :: (ConfigReader m, Typeable a) => ByteString -> m a
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
        contexts = map buildContexts plugs
        (amap, rmap) = buildPluginMaps acts in
    def { _contextActionMap = amap
        , _contextResultMap = rmap
        , _contextAgentConfig = def {_configPluginContexts = Map.fromList contexts}
        , _contextActionListeners = buildListeners plugs
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
    appendListener acc = _plugindefActionListeners >=> return . (++ acc)

addPlugin :: PluginDef -> PluginWriter
addPlugin pd = tell [pd]

definePlugin :: (Typeable a)
             => ByteString -> a -> Agent [ActionListener]
             -> ActionsWriter
             -> PluginDef
definePlugin pname pcontext alisteners pwriter
  = PluginDef { _plugindefName = pname
              , _plugindefContext = toDyn pcontext
              , _plugindefActions = execWriter pwriter
              , _plugindefActionListeners = alisteners
              }
