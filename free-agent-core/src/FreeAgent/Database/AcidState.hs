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
import           FreeAgent.Core

import           Control.Monad.State  (MonadState)

import Data.Acid
       (AcidState, IsAcidic, openLocalStateFrom, closeAcidState,
        makeAcidic, Query, Update, UpdateEvent, QueryEvent, EventResult )
import           Data.Acid.Local      ( createCheckpointAndClose)
import Data.Acid.Advanced (query', update', MethodState)
import           Data.Default (Default(..))

import Data.Typeable

data AcidBase = AcidBase {_baseAcid :: ()}
makeFields ''AcidBase

deriving instance Typeable1 AcidState

data AcidOptions = AcidOptions {checkPointOnClose :: Bool}

instance Default AcidOptions where
    def = AcidOptions True

openOrGetDb :: (MonadAgent m, IsAcidic s, Typeable s)
            => Text -> s -> AcidOptions -> m (AcidState s)
openOrGetDb name' init (AcidOptions needCheckpoint) =
    lookupResource name' >>= foundOpen
  where
    foundOpen (Just sh) = return sh
    foundOpen Nothing = doInit
    doInit = do
        path <- viewsConfig dbPath (</> convert name')
        acid' <- liftIO $ openLocalStateFrom path init
        manageResource name' acid' (closer acid')
        return acid'
    closer
      | needCheckpoint = createCheckpointAndClose
      | otherwise = closeAcidState

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
