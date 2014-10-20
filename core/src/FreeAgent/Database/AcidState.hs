{-# LANGUAGE FlexibleContexts, FunctionalDependencies, OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, TemplateHaskell #-}



module FreeAgent.Database.AcidState
    ( module FreeAgent.Database.AcidState
    , AcidState
    , makeAcidic
    , Query, Update
    , query', update'
    )

where

import FreeAgent.AgentPrelude
import FreeAgent.Core
import FreeAgent.Core.Action          ()
import FreeAgent.Core.Internal.Lenses

import Control.Monad.State            (MonadState)

import Data.Acid                      (AcidState, EventResult, IsAcidic, Query,
                                       QueryEvent, Update, UpdateEvent,
                                       closeAcidState, makeAcidic,
                                       openLocalStateFrom)
import Data.Acid.Advanced             (MethodState, query', update')
import Data.Acid.Memory               (openMemoryState)
import Data.Acid.Local                (createCheckpointAndClose)
import Data.Default                   (Default (..))

data AcidBase = AcidBase {_baseAcid :: ()}
makeFields ''AcidBase


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
        path <- viewConfig dbPath
        case path of --TODO: s/b an ADT
            "memory" -> do acid' <- liftIO $ openMemoryState init
                           manageResource name' acid' (return ())
                           return acid'
            _ -> let path' = convert (path </> convert name')
                 in do acid' <- liftIO $ {-# SCC "open-state" #-} openLocalStateFrom path' init
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
