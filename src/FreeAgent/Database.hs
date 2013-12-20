{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

module FreeAgent.Database
    ( module Database.LevelDB.Higher
    , module Database.LevelDB.Higher.Store
    , initAgentDB
    , withAgentDB
    , fromAgentDB
    , closeAgentDB
    , withSync
    )
where

import           FreeAgent.Prelude
import           FreeAgent.Lenses

import           Control.Concurrent.Chan.Lifted
import           Database.LevelDB.Higher
import           Database.LevelDB.Higher.Store

import           Control.Exception              (throw)
import           Control.Concurrent.Lifted      (forkOS, ThreadId)


-- Start the AgentDB background thread and comms channel
initAgentDB :: AgentContext -> IO (ThreadId, Chan DBMessage)
initAgentDB ctxt = do
    chan <- newChan
    thread <- forkOS $
        runCreateLevelDB (ctxt^.agentConfig.dbPath) "agent" $ loop chan
    return (thread, chan)
  where
    loop chan = do
        msg <- readChan chan
        case msg of
            Perform ma -> do
                void $ forkLevelDB ma
                loop chan
            Terminate -> return ()

-- Terminate the AgentDB background thread
closeAgentDB :: Chan DBMessage -> IO ()
closeAgentDB dbChan =
    writeChan dbChan Terminate

-- | Asynchronous perform an action on the database that will not
-- return a value - typically a put or delete
withAgentDB :: (AgentBase m, ConfigReader m) => AgentDB () -> m ()
withAgentDB ma = do
    dbChan <- _contextAgentDBChan <$> askConfig
    writeChan dbChan $ Perform ma

-- | Perform an action on the database and wait for a result. Can be
-- used to confirm a put/delete succeeded but you also need to set the
-- database writee option to use a sync operation (e.g. use 'withSync')
fromAgentDB :: (AgentBase m, ConfigReader m) => AgentDB a -> m a
fromAgentDB ma = do
    dbChan <- _contextAgentDBChan <$> askConfig
    result <- newEmptyMVar
    writeChan dbChan $ Perform $
        catchAny ( do !a <- ma --TODO: use NFData
                      putMVar result a)
                 (\exception -> do
                     logM $ "fromAgentDB action exception: " ++ tshow exception
                     putMVar result (throw exception) )
    takeMVar result

-- | Set the write option sync = True for a block of database operations
withSync :: (MonadLevelDB m) => m () -> m()
withSync = withOptions (def, def {sync = True})
