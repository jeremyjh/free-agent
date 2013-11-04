{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Dash.Core
    ( runAgent
    , spawnAgent
    , mapAgent
    , viewConfig
    ) where

import          Dash.Prelude
import          Dash.Lenses
import          Database.LevelDB.Higher (mapLevelDBT, runCreateLevelDB)
import          Control.Monad.Reader
import          Control.Exception
import          Control.Distributed.Process
import          Control.Distributed.Process.Node
import          Network.Transport.TCP
import          Network.Transport (closeTransport)

-- | Execute the agent - main entry point
runAgent :: AgentConfig -> Agent () -> IO ()
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


-- | Use a lens to view a portion of AgentConfig
viewConfig :: (ConfigReader m) => Getting a AgentConfig a -> m a
viewConfig lens = do
    conf <- askConfig
    return $ view lens conf
