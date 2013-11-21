{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Action
    ( fetchAction, scanActions, decodeAction, stashAction, stash
    , withActionKS
    , register, actionType
    , registerAll
    , registerActionMap
    , actionResult, extractResult, toAction
    )
where

import           FreeAgent.Prelude
import qualified Prelude                 as P
import           FreeAgent.Lenses

import qualified Data.ByteString.Char8   as BS
import           Data.SafeCopy
import           Data.Serialize          (Serialize)
import qualified Data.Serialize          as Cereal
import           Data.Binary             (Binary)
import qualified Data.Binary             as Binary
import           Data.Dynamic
    (toDyn, fromDynamic, dynTypeRep)

import           Control.Monad.Base (MonadBase)
import           Database.LevelDB.Higher

import qualified Data.Map                as Map

import           Control.Monad.Writer    (runWriter, tell)

import           System.IO.Unsafe (unsafePerformIO)
import           Control.Distributed.Process.Lifted (send)


-- Serialization instances for Action are all this module as they require specialized
-- and sensitive functions we want to keep encapsulated

instance Binary Action where
    put (Action a _) = Binary.put $ wrap a
    get = do
        wk <- Binary.get
        wt  <- Binary.get
        wv <- Binary.get
        return $ decodeAction' readActionMap (WrappedAction wk wt wv)

instance Serialize Action where
    put (Action a _) = Cereal.put $ wrap a
    get = do
        wk <- Cereal.get
        wt  <- Cereal.get
        wv <- Cereal.get
        return $ decodeAction' readActionMap (WrappedAction wk wt wv)

instance SafeCopy Action where
    putCopy (Action a _) = putCopy a

instance Eq Action where
    a == b =  Cereal.encode a == Cereal.encode b

instance Stashable Action where
    key (Action a _) = key a

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
    case Cereal.decode bs of
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
register :: forall a b. (Actionable a b)
         => a -> ActionsWriter
register act = tell [(fqName act, unWrapAction (unWrap :: ActionUnwrapper a))]

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

decodeAction' :: ActionMap -> WrappedAction -> Action
decodeAction' pluginMap wrapped = do
    case Map.lookup (_wrappedTypeName wrapped) pluginMap of
        Just f -> let (Right a) = f wrapped in a
        Nothing -> error $ "Type Name: " ++ BS.unpack (_wrappedTypeName wrapped)
                    ++ " not matched! Is your plugin registered?"

-- | Set or re-set the top-level Action Map (done after plugins are registered)
registerActionMap :: (MonadBase IO m) => ActionMap -> m ()
registerActionMap = writeIORef globalActionMap

--TODO move this to Action module
globalActionMap :: IORef ActionMap
globalActionMap = unsafePerformIO $ newIORef (Map.fromList [])
{-# NOINLINE globalActionMap #-}

-- Yes...we are unsafe. This is necessary to transparently deserialize
-- Actions defined in Plugins which are registered at runtime. There are workarounds for
-- most cases but to receive an Action in a Cloud Haskell 'expect' we must be
-- able to deserialize it directly.
readActionMap :: ActionMap
readActionMap = unsafePerformIO $ readIORef globalActionMap
{-# NOINLINE readActionMap #-}

-- | Unwrap a concrete type into an Action
--
unWrapAction :: (Actionable a b) =>
                ActionUnwrapper a -> WrappedAction -> FetchAction
unWrapAction uw wrapped = Action <$> uw wrapped <*> pure (undefined :: b)

-- Wrap a concrete type for stash or send where it
-- will be decoded to an Action or ActionResult
wrap :: (Stashable a) => a -> WrappedAction
wrap st = WrappedAction (key st) (fqName st) (Cereal.encode st)

-- | Unwrap a 'Wrapper' into a (known) concrete type
unWrap :: (Stashable a) => WrappedAction -> Either FetchFail a
unWrap = decodeStore . _wrappedValue

-- | Wrap a value in the ActionResult box
actionResult :: (Stashable a, Serializable a, Show a) => a -> ActionResult
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

-- same as Action - we need ActionResult serialization instances here
instance Binary ActionResult where
    put (ActionResult a _) = Binary.put a
    get = error "You cannot decode to the ActionResult existential type."

instance Cereal.Serialize ActionResult where
    get = undefined
    put = undefined

instance SafeCopy ActionResult where

-- ActionResult instance overrides the underlying implementation
-- there is no reason for an different implementation to ever be used than
-- the default, but this ensures ActionResults are all sent the same way
instance Resulting ActionResult where
    deliver (ActionResult a _) p = send p a
    extract (ActionResult _ d) = d

instance Runnable Action ActionResult where
    exec (Action a _) = do
        execR <- exec a
        return $ flip fmap execR $ \result ->
                ActionResult result (toDyn result)

instance Stashable ActionResult where
    key (ActionResult a _) = key a
