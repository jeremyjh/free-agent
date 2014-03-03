{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Database
    ( module Database.LevelDB.Higher
    , module Database.LevelDB.Higher.Store
    , initAgentDB
    , asyncAgentDb
    , agentDb
    , closeAgentDB
    )
where

import           FreeAgent.Lenses
import           AgentPrelude

import           Control.Exception              (throw)

import           Control.Concurrent.Chan.Lifted
import           Control.Concurrent.Lifted      (ThreadId, forkOS)
import           Control.Monad.Logger           (LoggingT (..),
                                                 runStdoutLoggingT)
import           Database.LevelDB.Higher hiding (deriveStorable)
import           Database.LevelDB.Higher.Store hiding (deriveStorable)


instance (ContextReader m) => ContextReader (LoggingT m)
    where askContext = lift askContext

instance MonadLevelDB (LoggingT (LevelDBT IO))
    where
      get = lift . get
      put k v = lift $ put k v
      delete = lift . delete
      liftLevelDB = lift . liftLevelDB
      withDBContext f = mapLoggingT (withDBContext f)
      --TODO: debug log count is hardcoded - need to newtype AgentDB
      -- and include ContextReader so it works like Agent logging
        where mapLoggingT f' = lift . f' . runAgentLoggingT 10


-- Start the AgentDB background thread and comms channel
initAgentDB :: AgentContext -> IO (ThreadId, Chan DBMessage)
initAgentDB ctxt = do
    chan <- newChan
    thread <- forkOS $
        runLevelDB (ctxt^.agentConfig.dbPath)
                   def {createIfMissing = True}
                   (def, def {sync=True})
                   "agent" $ loop chan
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
asyncAgentDb :: (AgentBase m, ContextReader m) => AgentDB () -> m ()
asyncAgentDb ma = viewConfig agentDBChan >>=
    flip writeChan (Perform $
        catchAny (withOptions def ma)
                 (\exception ->
                     $(logWarn) $ "asyncAgentDb exception: " ++ tshow exception))


-- | Perform an action on the database and wait for a result. Can be
-- used to confirm a put/delete succeeded but you also need to set the
-- database writee option to use a sync operation (e.g. use 'withSync')
agentDb :: (AgentBase m, ContextReader m, NFData a) => AgentDB a -> m a
agentDb ma = do
    dbChan <- viewConfig agentDBChan
    result <- newEmptyMVar
    writeChan dbChan $ Perform $
        catchAny (do  !a <- ma
                      return $ rnf a
                      putMVar result a )
                 (\exception -> do
                     $(logWarn) $ "agentDb exception: " ++ tshow exception
                     putMVar result (throw exception) )
    takeMVar result
