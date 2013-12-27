{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

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

import           Control.Monad.Logger           (MonadLogger, LoggingT(..), runStdoutLoggingT)

instance (ConfigReader m) => ConfigReader (LoggingT m)
    where askConfig = lift askConfig

instance MonadLevelDB (LoggingT (LevelDBT IO))
    where
      get = lift . get
      put k v = lift $ put k v
      delete = lift . delete
      liftLevelDB = lift . liftLevelDB
      withDBContext f = mapLoggingT (withDBContext f)
      --TODO: debug log count is hardcoded - need to newtype AgentDB
      -- and include ConfigReader so it works like Agent logging
        where mapLoggingT f' = lift . f' . runAgentLoggingT 10


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
                --TODO: also need to clean this up with AgentDB newtype
                void $ forkLevelDB $ runStdoutLoggingT ma
                loop chan
            Terminate -> return ()

-- Terminate the AgentDB background thread
closeAgentDB :: Chan DBMessage -> IO ()
closeAgentDB dbChan =
    writeChan dbChan Terminate

-- | Asynchronous perform an action on the database that will not
-- return a value - typically a put or delete
withAgentDB :: (AgentBase m, ConfigReader m) => AgentDB () -> m ()
withAgentDB ma = viewConfig agentDBChan >>=
    (flip writeChan $ Perform $
        catchAny (ma)
                 (\exception -> do
                     $(logWarn) $ "withAgentDB exception: " ++ tshow exception))


-- | Perform an action on the database and wait for a result. Can be
-- used to confirm a put/delete succeeded but you also need to set the
-- database writee option to use a sync operation (e.g. use 'withSync')
fromAgentDB :: (AgentBase m, ConfigReader m) => AgentDB a -> m a
fromAgentDB ma = do
    dbChan <- viewConfig agentDBChan
    result <- newEmptyMVar
    writeChan dbChan $ Perform $
        catchAny (do  !a <- ma --TODO: use NFData
                      putMVar result a)
                 (\exception -> do
                     $(logWarn) $ "fromAgentDB exception: " ++ tshow exception
                     putMVar result (throw exception) )
    takeMVar result

-- | Set the write option sync = True for a block of database operations
withSync :: (MonadLogger m, MonadLevelDB m) => m () -> m()
withSync = withOptions (def, def {sync = True})
