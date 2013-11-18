{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Action
    ( fetchAction, scanActions, decodeAction, stashAction, stash
    , withActionKS
    , register, actionType
    , registerAll
    , actionResult, extractResult, toAction
    )
where

import           FreeAgent.Prelude
import qualified Prelude                 as P
import           FreeAgent.Lenses
import           FreeAgent.Core

import           Data.Serialize          (decode)
import           Data.Dynamic
    (toDyn, fromDynamic, dynTypeRep)

import           Database.LevelDB.Higher

import qualified Data.Map                as Map

import           Control.Monad.Writer    (runWriter, tell)




-- | Fix the type & keyspace to Action for fetch
fetchAction :: (MonadLevelDB m, ConfigReader m)
            => Key -> m FetchAction
fetchAction = withActionKS . fetch

-- | Fix the type & keyspace to Action for fetch
stashAction :: (MonadLevelDB m)
            => Action -> m ()
stashAction = withActionKS . stash

-- | Fix the keyspace to 'withActionKS' and decodes each
-- result into an Action.
scanActions :: (MonadLevelDB m, ConfigReader m)
             => Key -> m [FetchAction]
scanActions prefix = withActionKS $
    scan prefix queryList { scanMap = decodeAction . snd }

-- | Deserializes and unWraps the underlying type using
-- the registered 'actionType'for it
decodeAction :: ByteString -> FetchAction
decodeAction bs =
    case decode bs of
        Right a -> Right a
        Left s -> Left $ ParseFail s

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

withActionKS :: (MonadLevelDB m) => m a -> m a
withActionKS = withKeySpace "agent:actions"
