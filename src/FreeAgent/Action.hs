{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Action
    ( fetchAction, scanActions, decodeAction, stashAction, stash
    , deleteAction
    , withActionKS
    , register, actionType
    , registerPluginMaps
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

import           Database.LevelDB.Higher

import qualified Data.Map                as Map

import           Control.Monad.Writer    (tell)

import           System.IO.Unsafe (unsafePerformIO)

-- Serialization instances for Action are all this module as they require specialized
-- and sensitive functions we want to keep encapsulated e.g. (readPluginMaps)

deriving instance Eq ResultSummary
deriveSerializers ''ResultSummary

instance Binary Action where
    put (Action a) = Binary.put $ wrap a
    get = do
        wrapped <- getWrappedBinary
        return $ decodeAction' (fst readPluginMaps) wrapped

instance Serialize Action where
    put (Action a) = Cereal.put $ wrap a
    get = do
        wrapped <- getWrappedCereal
        return $ decodeAction' (fst readPluginMaps) wrapped

instance SafeCopy Action where
    putCopy (Action a) = putCopy a

instance Eq Action where
    a == b =  Cereal.encode a == Cereal.encode b

instance Stashable Action where
    key (Action a) = key a

-- same as Action - we need ActionResult serialization instances here
instance Binary ActionResult where
    put (ActionResult a) = Binary.put $ wrap a
    get = do
        wrapped <- getWrappedBinary
        return $ decodeResult' (snd readPluginMaps) wrapped

instance Serialize ActionResult where
    put (ActionResult a) = Cereal.put $ wrap a
    get = do
        wrapped <- getWrappedCereal
        return $ decodeResult' (snd readPluginMaps) wrapped

instance SafeCopy ActionResult where

instance Resulting ActionResult where
    extract (ActionResult a) = cast a
    summary (ActionResult a) = summary a

instance Runnable Action ActionResult where
    exec (Action a) = do
        execR <- exec a
        return $ flip fmap execR $ \result ->
                ActionResult result
    matches f (Action a)  =
        case cast a of
            Just a' -> f a'
            Nothing -> False

instance Stashable ActionResult where
    key (ActionResult a) = key a
-- | Fix the type & keyspace to Action for fetch
fetchAction :: Key -> AgentDB FetchAction
fetchAction = withActionKS . fetch

-- | Fix the keyspace to Action for stash
stashAction :: Action -> AgentDB ()
stashAction = withActionKS . stash

-- | Fix the keyspace to Action for delete
deleteAction :: Key -> AgentDB ()
deleteAction = withActionKS . delete

-- | Fix the keyspace to 'withActionKS' and decodes each
-- result using 'decodeAction'.
--
scanActions :: AgentContext -> Key -> AgentDB [FetchAction]
scanActions ctxt prefix = withActionKS $
    let pm = _contextActionMap ctxt
        decoder = decodeAction pm in
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
                     , unWrapAction (unWrap :: Unwrapper a)
                     , fqName (undefined :: b)
                     , unwrapResult (unWrap :: Unwrapper b) )]


-- | Used only to fix the type passed to 'register' - this should not
-- ever be evaluated and will throw an error if it is
actionType :: (Actionable a b) => a
actionType = error "actionType should never be evaluated! Only pass it \
                   \ to register which takes the TypeRep but does not evaluate it."

-- | Unwrap a concrete type into an Action
--
unWrapAction :: (Actionable a b)
             => Unwrapper a -> Wrapped -> FetchAction
unWrapAction uw wrapped = Action <$> uw wrapped

unwrapResult :: (Resulting b)
             => Unwrapper b -> Wrapped -> Either FetchFail ActionResult
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
registerPluginMaps :: (MonadBase IO m) => PluginMaps -> m ()
registerPluginMaps = writeIORef globalPluginMaps

--TODO move this to Action module
globalPluginMaps :: IORef PluginMaps
globalPluginMaps = unsafePerformIO $ newIORef (Map.fromList [], Map.fromList [])
{-# NOINLINE globalPluginMaps #-}

-- Yes...we are unsafe. This is necessary to transparently deserialize
-- Actions defined in Plugins which are registered at runtime. There are workarounds for
-- most cases but to receive an Action in a Cloud Haskell 'expect' we must be
-- able to deserialize it directly.
readPluginMaps :: PluginMaps
readPluginMaps = unsafePerformIO $ readIORef globalPluginMaps
{-# NOINLINE readPluginMaps #-}

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
