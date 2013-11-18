{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Action
    ( fetchAction, scanActions, decodeAction, stash
    , register, actionType
    , registerAll
    , actionResult, extractResult, toAction
    )
where

import           FreeAgent.Prelude
import qualified Prelude                 as P
import           FreeAgent.Lenses
import           FreeAgent.Core

import           Data.Dynamic
    (toDyn, fromDynamic, dynTypeRep)

import           Database.LevelDB.Higher

import qualified Data.Map                as Map

import           Control.Monad.Writer    (runWriter, tell)




-- | Like Higher.Store.fetch for an action using 'decodeAction' to deserialize
-- 'Wrapper' types using the 'register' 'actionType'
fetchAction :: (MonadLevelDB m, ConfigReader m)
            => Key -> m FetchAction
fetchAction k = do
    pm <- viewConfig actionMap
    wrapped <- get k
    return $ case wrapped of
        Nothing -> Left $ NotFound (showStr k)
        Just bs -> decodeAction pm bs

-- | All keys from this keyspace are actions
scanActions :: (MonadLevelDB m, ConfigReader m)
             => Key -> m [FetchAction]
scanActions prefix = do
    pm <- viewConfig actionMap
    let decoder = decodeAction pm
    scan prefix queryList { scanMap = decoder . snd }

-- | Save a serializable type with an instance for Stash
-- which provides the key.
--
stash :: (MonadLevelDB m, Stashable s)
      => s -> m ()
stash s = store (key s) s


-- | Use to register your Action types so they can be
-- deserialized dynamically at runtime; invoke as:
--
-- > register (actiontype :: MyType)
register :: (Actionable a b)
         => a -> ActionsWriter
register act = tell [(fqName act, unWrapAction (anUnWrap act))]
  where
      -- This fixes the type for unWrap to be that of the top-level param
      -- This way we get the concrete decode method we need to deserialize
      -- And yet, we bury it in the Existential Action
      anUnWrap :: (Stashable a, Runnable a b) => a -> ActionUnwrapper a
      anUnWrap _ = unWrap

-- | Combine the results of multiple ActionsWriters from different plugins
registerAll :: ActionsWriter -> ActionMap
registerAll = Map.fromList . snd . runWriter

-- | Used only to fix the type passed to 'register' - this should not
-- ever be evaluated and will throw an error if it is
actionType :: (Actionable a b) => a
actionType = error "actionType should never be evaluated! Only pass it \
                   \ to register which takes the TypeRep but does not evaluate it."

-- | Box an Actionable in an Action - saves the hassle of having to provide the
-- superfulous result parameter to the Action constructor.
toAction :: (Actionable a b) => a -> Action
toAction a = Action a (undefined :: b)

-- | Unwrap a concrete type into an Action
--
unWrapAction :: (Actionable a b) =>
                ActionUnwrapper a -> WrappedAction -> FetchAction
unWrapAction uw wrapped = Action <$> uw wrapped <*> pure (undefined :: b)

-- | Unwrap a 'Wrapper' into a (known) concrete type
unWrap :: (Stashable a) => WrappedAction -> Either FetchFail a
unWrap = decodeStore . _wrappedValue

-- | Wrap a value in the ActionResult box
actionResult :: (Serializable a, Show a) => a -> ActionResult
actionResult a = ActionResult a (toDyn a)

-- | Extract a concrete type from an ActionResult. Will throw an exception
-- if the required type does not match.
extractResult :: (Typeable a) => ActionResult -> a
extractResult a =
    case fromDynamic $ extract a of
        (Just v) -> v
        Nothing -> error $ "Cannot extract ActionResult of " ++ repName ++ "to the expected type."
  where
    repName = show $ dynTypeRep $ extract a
