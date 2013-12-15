{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module FreeAgent.Database
    ( module Database.LevelDB.Higher
    , module Database.LevelDB.Higher.Store
    , initAgentDB
    , withAgentDB
    , fromAgentDB
    , closeAgentDB
    )
where

import           FreeAgent.Prelude
import           FreeAgent.Lenses

import           Control.Concurrent.Chan.Lifted
import           Database.LevelDB.Higher
import           Database.LevelDB.Higher.Store

import           Control.Concurrent.Lifted      (forkOS, ThreadId)


initAgentDB :: AgentContext -> IO (ThreadId, Chan DBMessage)
initAgentDB ctxt = do
    chan <- newChan
    thread <- forkOS $
        runCreateLevelDB (ctxt^.agentConfig.dbPath) "agent" $ mainLoop chan
    return (thread, chan)

closeAgentDB :: Chan DBMessage -> IO ()
closeAgentDB dbChan =
    writeChan dbChan Terminate

withAgentDB :: (AgentBase m, ConfigReader m) => AgentDB () -> m ()
withAgentDB ma = do
    dbChan <- _contextAgentDBChan <$> askConfig
    writeChan dbChan $ Perform ma

fromAgentDB :: (AgentBase m, ConfigReader m) => AgentDB a -> m a
fromAgentDB ma = do
    dbChan <- _contextAgentDBChan <$> askConfig
    result <- newEmptyMVar
    writeChan dbChan $ Perform $
        catchAny (ma >>= putMVar result)
                 (\exception -> do
                     putStrLn $ "Database operation failed: " ++ tshow exception
                     putMVar result (error $ "database operation failed: " ++ show exception) )
    takeMVar result

mainLoop :: Chan DBMessage -> AgentDB ()
mainLoop chan = do
    msg <- readChan chan
    case msg of
        Perform ma -> do
            void $ forkLevelDB ma
            mainLoop chan
        Terminate -> return ()
