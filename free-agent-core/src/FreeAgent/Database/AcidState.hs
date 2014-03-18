{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Database.AcidState
    ( module FreeAgent.Database.AcidState
    , AcidState
    , makeAcidic
    , Query, Update
    )

where

import           AgentPrelude hiding ((</>))
-- TODO: use CurrentOS Filepath
import           System.FilePath.Posix ((</>))
import           FreeAgent.Lenses
import           FreeAgent.Action     ()

import qualified Data.Map.Strict      as Map
import           Control.Monad.State  (MonadState)

import Data.Acid
       (AcidState, IsAcidic, openLocalStateFrom, closeAcidState,
        makeAcidic, Query, Update, UpdateEvent, QueryEvent, EventResult )
import           Data.Acid.Local      ( createCheckpointAndClose)
import Data.Acid.Advanced (query', update', MethodState)
import           Data.Default (Default(..))
import Data.Dynamic (fromDynamic, toDyn)

import Data.Typeable

data AcidBase = AcidBase {_baseAcid :: ()}
makeFields ''AcidBase

deriving instance Typeable1 AcidState

data AcidOptions = AcidOptions {checkPointOnClose :: Bool}

instance Default AcidOptions where
    def = AcidOptions True

openOrGetDb :: (MonadAgent m, IsAcidic s, Typeable s)
            => String -> s -> AcidOptions -> m (Maybe (AcidState s))
openOrGetDb name' init (AcidOptions needCheckpoint) =
    lookupOpen name' >>= foundOpen >>= extractState
  where
    foundOpen (Just sh) = return  sh
    foundOpen Nothing = doInit
    extractState (StateHandles ac _) = return $ fromDynamic ac
    doInit = do
        path <- viewsConfig (agentConfig.dbPath) (</> name')
        ast <- liftIO $ openLocalStateFrom path init
        let newhandle = StateHandles (toDyn ast)  (const (closer ast))
        handlesMV <- viewConfig openStates
        modifyMVar_ handlesMV $ \handles ->
            return $ Map.insert name' newhandle handles
        return newhandle
    closer = if needCheckpoint then createCheckpointAndClose
             else closeAcidState

closeAllStates :: (AgentBase m) => MVar (Map String StateHandles) -> m ()
closeAllStates statesMV = do
    handles <-  takeMVar statesMV
    forM_ handles $ \ (StateHandles _ closeFn) ->
        liftIO $ closeFn ()

lookupOpen :: MonadAgent m => String -> m (Maybe StateHandles)
lookupOpen name' = do
    mvstates <- viewConfig openStates
    states <- readMVar mvstates
    return $ Map.lookup name' states

query :: (QueryEvent e, MonadIO m, MonadState s m, HasAcid s (AcidState (MethodState e)))
      => e -> m (EventResult e)
query event = do
    as <- use acid
    query' as event

update :: (UpdateEvent e, MonadIO m, MonadState s m, HasAcid s (AcidState (MethodState e)))
       => e -> m (EventResult e)
update event =
    do as <- use acid
       update' as event
