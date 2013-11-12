{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Core
    ( runAgent
    , spawnAgent
    , mapAgent
    , viewConfig
    , extractConfig
    , definePlugin
    , registerPlugins
    , addPlugin
    ) where

import           FreeAgent.Prelude
import           FreeAgent.Lenses
import           Control.Monad.Writer             (execWriter, tell)
import           Database.LevelDB.Higher (mapLevelDBT, runCreateLevelDB)
import           Control.Monad.Reader
import           Control.Exception
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Network.Transport.TCP
import           Network.Transport (closeTransport)

import qualified Data.Map as Map
import           Data.Dynamic (toDyn, fromDynamic)

-- | Execute the agent - main entry point
runAgent :: AgentContext -> Agent () -> IO ()
runAgent config ma = do
    let lbt = runReaderT (unAgent ma) config
        proc = runCreateLevelDB (config^.dbPath) "agent" lbt
    eithertcp <- createTransport (config^.nodeHost) (config^.nodePort) defaultTCPParameters
    case eithertcp of
        Right tcp -> do
            node <- newLocalNode tcp initRemoteTable
            runProcess node proc
            closeTransport tcp
        Left msg -> throw msg

-- | Agent version of 'Process' 'spawnLocal'
spawnAgent :: Agent () -> Agent ProcessId
spawnAgent = mapAgent spawnLocal

-- | Map over the underlying Process monad
mapAgent :: (Process a -> Process b) -> Agent a -> Agent b
mapAgent f ma = Agent $
    mapReaderT (mapLevelDBT f) (unAgent ma)


-- | Use a lens to view a portion of AgentContext
viewConfig :: (ConfigReader m) => Getting a AgentContext a -> m a
viewConfig lens = do
    conf <- askConfig
    return $ view lens conf

-- | Lookup a plugin-specific config from the pluginConfigs map
-- and extract it to a concrete type with fromDynamic
extractConfig :: (ConfigReader m, Typeable a) => ByteString -> m a
extractConfig configName = do
    configMap <- viewConfig pluginContexts
    case Map.lookup configName configMap of
        Nothing ->
            error (show (configName ++ " not found! Did you register the plugin config?"))
        Just dynconf ->
            case fromDynamic dynconf of
                Nothing -> error "fromDynamic failed for NagiosConfig!"
                Just nagconf -> return nagconf

registerPlugins :: PluginWriter -> AgentContext
registerPlugins pw =
    let plugins = execWriter pw
        actions = concat $ map _plugindefActions plugins
        contexts = map buildContexts plugins
        in
        def { _configActionMap = Map.fromList actions
            , _configPluginContexts = Map.fromList contexts
            }
  where
    buildContexts plugin =
        (_plugindefName plugin, _plugindefContext plugin)

addPlugin :: PluginDef -> PluginWriter
addPlugin pd = tell [pd]

definePlugin :: (Typeable a)
             => ByteString -> a -> ActionsWriter -> PluginDef
definePlugin name context pw
  = PluginDef { _plugindefName = name
              , _plugindefContext = toDyn context
              , _plugindefActions = execWriter pw
              }
