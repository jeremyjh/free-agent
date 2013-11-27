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
    , registerActionMap
    , toAction
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
import           Data.Dynamic            (cast)

import           Control.Monad.Base (MonadBase)
import           Database.LevelDB.Higher

import qualified Data.Map                as Map

import           Control.Monad.Writer    (tell)

import           System.IO.Unsafe (unsafePerformIO)
import           Control.Distributed.Process.Lifted (send)


-- Serialization instances for Action are all this module as they require specialized
-- and sensitive functions we want to keep encapsulated


instance Binary Action where
    put (Action a _) = Binary.put $ wrap a
    get = do
        wrapped <- getWrappedBinary
        return $ decodeAction' (fst readActionMap) wrapped

instance Serialize Action where
    put (Action a _) = Cereal.put $ wrap a
    get = do
        wrapped <- getWrappedCereal
        return $ decodeAction' (fst readActionMap) wrapped

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
-- result using 'decodeAction'.
--
scanActions :: (MonadLevelDB m, ConfigReader m)
             => Key -> m [FetchAction]
scanActions prefix = withActionKS $ do
    pm <- _configActionMap <$> askConfig
    let decoder = decodeAction pm
    scan prefix queryList { scanMap = decoder . snd }

-- | Decode a Cerealized Wrapped Action - unlike the decode implementation
-- for 'Action' this function will not throw an exception on parse or lookup failures
decodeAction :: ActionMap -> ByteString -> FetchAction
decodeAction pluginMap bs =
    case Cereal.decode bs of
        Left s -> Left $ ParseFail s
        Right wrapped ->
            case Map.lookup (wrapped^.typeName) pluginMap of
                Just uw -> uw wrapped
                Nothing -> Left $ ParseFail $ "Type Name: " ++ BS.unpack (wrapped^.typeName)
                            ++ " not matched! Is your plugin registered?"

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
register :: forall a b. (Actionable a b, Resulting b)
         => a -> ActionsWriter
register act = tell [( fqName act
                     , unWrapAction (unWrap :: ActionUnwrapper a)
                     , fqName (undefined :: b)
                     , unwrapResult (unWrap :: ActionUnwrapper b) )]


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
unWrapAction :: (Actionable a b)
             => ActionUnwrapper a -> Wrapped -> FetchAction
unWrapAction uw wrapped = Action <$> uw wrapped <*> pure (undefined :: b)

unwrapResult :: (Resulting b)
             => ActionUnwrapper b -> Wrapped -> Either FetchFail ActionResult
unwrapResult uw wrapped = ActionResult <$> uw wrapped

-- Wrap a concrete type for stash or send where it
-- will be decoded to an Action or ActionResult
wrap :: (Stashable a) => a -> Wrapped
wrap st = Wrapped (key st) (fqName st) (Cereal.encode st)

-- | Unwrap a 'Wrapper' into a (known) concrete type
unWrap :: (Stashable a) => Wrapped -> Either FetchFail a
unWrap = decodeStore . _wrappedValue

--used in serialization instances - throws an exception since Binary decode is no maybe
decodeAction' :: ActionMap -> Wrapped -> Action
decodeAction' pluginMap wrapped =
    case Map.lookup (wrapped^.typeName) pluginMap of
        Just f -> case f wrapped of
            Right act -> act
            Left (ParseFail s) -> error $ "Error deserializing wrapper: " ++ s
            Left _ -> error "Unknown error deserializing wrapper"
        Nothing -> error $ "Type Name: " ++ BS.unpack (wrapped^.typeName)
                    ++ " not matched! Is your plugin registered?"

-- | Set or re-set the top-level Action Map (done after plugins are registered)
registerActionMap :: (MonadBase IO m) => PluginMaps -> m ()
registerActionMap = writeIORef globalActionMap

--TODO move this to Action module
globalActionMap :: IORef PluginMaps
globalActionMap = unsafePerformIO $ newIORef (Map.fromList [], Map.fromList [])
{-# NOINLINE globalActionMap #-}

-- Yes...we are unsafe. This is necessary to transparently deserialize
-- Actions defined in Plugins which are registered at runtime. There are workarounds for
-- most cases but to receive an Action in a Cloud Haskell 'expect' we must be
-- able to deserialize it directly.
readActionMap :: PluginMaps
readActionMap = unsafePerformIO $ readIORef globalActionMap
{-# NOINLINE readActionMap #-}

withActionKS :: (MonadLevelDB m) => m a -> m a
withActionKS = withKeySpace "agent:actions"

decodeResult' :: ResultMap -> Wrapped -> ActionResult
decodeResult' pluginMap wrapped =
    case Map.lookup (wrapped^.typeName) pluginMap of
        Just f -> case f wrapped of
            Right act -> act
            Left (ParseFail s) -> error $ "Error deserializing wrapper: " ++ s
            Left _ -> error "Unknown error deserializing wrapper"
        Nothing -> error $ "Type Name: " ++ BS.unpack (wrapped^.typeName)
                    ++ " not matched! Is your plugin registered?"

-- same as Action - we need ActionResult serialization instances here
instance Binary ActionResult where
    put (ActionResult a) = Binary.put $ wrap a
    get = do
        wrapped <- getWrappedBinary
        return $ decodeResult' (snd readActionMap) wrapped

instance Serialize ActionResult where
    put (ActionResult a) = Cereal.put $ wrap a
    get = do
        wrapped <- getWrappedCereal
        return $ decodeResult' (snd readActionMap) wrapped

instance SafeCopy ActionResult where

-- ActionResult instance overrides the underlying implementation
-- there is no reason for an different implementation to ever be used than
-- the default, but this ensures ActionResults are all sent the same way
instance Resulting ActionResult where
    deliver (ActionResult a) p = send p a
    extract (ActionResult a) = cast a

instance Runnable Action ActionResult where
    exec (Action a _) = do
        execR <- exec a
        return $ flip fmap execR $ \result ->
                ActionResult result

instance Stashable ActionResult where
    key (ActionResult a) = key a

getWrappedBinary :: Binary.Get Wrapped
getWrappedBinary = do
        wk <- Binary.get
        wt  <- Binary.get
        wv <- Binary.get
        return (Wrapped wk wt wv)

getWrappedCereal :: Cereal.Get Wrapped
getWrappedCereal = do
        wk <- Cereal.get
        wt  <- Cereal.get
        wv <- Cereal.get
        return (Wrapped wk wt wv)
