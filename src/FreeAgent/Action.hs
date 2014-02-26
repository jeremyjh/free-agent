{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Action
    ( fetchAction, scanActions, decodeAction, stashAction, stash
    , toAction
    , deleteAction
    , register, actionType
    , registerPluginMaps
    )
where

import           AgentPrelude
import qualified FreeAgent.Database.KeySpace as KS
import           FreeAgent.Lenses

import           Control.Monad.Writer        (tell)
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Char8       as BS
import           Data.Dynamic                (cast)
import qualified Data.Map                    as Map
import qualified Prelude                     as P
import           System.IO.Unsafe            (unsafePerformIO)

import           Data.SafeCopy
import           Data.Serialize              (Serialize)
import qualified Data.Serialize              as Cereal
import           Database.LevelDB.Higher

-- Serialization instances for Action are all this module as they require specialized
-- and sensitive functions we want to keep encapsulated e.g. (readPluginMaps)

deriving instance Eq ResultSummary
deriveSerializers ''ResultSummary

instance Binary Action where
    put (Action a) = Binary.put $ wrap a
    get = do
        wrapped <- Binary.get
        return $ decodeAction' (fst readPluginMaps) wrapped

instance Serialize Action where
    put = safePut
    get = safeGet

instance SafeCopy Action where
    version = 1
    kind = base
    errorTypeName _ = "FreeAgent.Type.Action"
    putCopy (Action a) = contain $ safePut $ wrap a
    getCopy = contain $ do
        wrapped <- safeGet
        return $ decodeAction' (fst readPluginMaps) wrapped

instance Stashable Action where
    key (Action a) = key a

-- same as Action - we need Result serialization instances here
instance Binary Result where
    put (Result a) = Binary.put $ wrap a
    get = do
        wrapped <- Binary.get
        return $ decodeResult' (snd readPluginMaps) wrapped

instance Serialize Result where
    put = safePut
    get = safeGet

instance SafeCopy Result where
    version = 1
    kind = base
    errorTypeName _ = "FreeAgent.Types.Result"
    putCopy (Result r) = contain $ safePut (wrap r)
    getCopy = contain $ do
        wrapped <- safeGet
        return $ decodeResult' (snd readPluginMaps) wrapped

instance Resulting Result where
    extract (Result a) = cast a
    summary (Result a) = summary a
    matchR f (Result a)  = maybe False f (cast a)

instance Runnable Action Result where
    exec (Action a) = do
        execR <- exec a
        return $ flip fmap execR $ \result ->
                Result result
    matchA f (Action a)  = maybe False f (cast a)

instance Stashable Result where
    key (Result a) = key a
-- | Fix the type & keyspace to Action for fetch
fetchAction :: MonadLevelDB m => Key -> m FetchAction
fetchAction = withActionKS . fetch

-- | Fix the keyspace to Action for stash
stashAction :: MonadLevelDB m => Action -> m Action
stashAction = withActionKS . stash

-- | Fix the keyspace to Action for delete
deleteAction :: MonadLevelDB m => Key -> m ()
deleteAction = withActionKS . delete

-- | Fix the keyspace to 'withActionKS' and decodes each
-- result using 'decodeAction'.
--
scanActions :: MonadLevelDB m => Key -> m [FetchAction]
scanActions prefix = withActionKS $
    scan prefix queryList { scanMap = decodeAction . snd }

decodeAction :: ByteString -> FetchAction
decodeAction bs = either (Left . ParseFail) Right (Cereal.decode bs)

-- | Wrap a concrete action in existential unless it is already an Action
toAction :: (Actionable a b) => a -> Action
toAction act = fromMaybe (Action act) (cast act)

-- | Save a serializable type with an instance for Stash
-- which provides the key - returns the same input.
stash :: (MonadLevelDB m, Stashable a)
      => a -> m a
stash s = do store (key s) s; return s

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
             => Unwrapper b -> Wrapped -> Either FetchFail Result
unwrapResult uw wrapped = Result <$> uw wrapped

-- Wrap a concrete type for stash or send where it
-- will be decoded to an Action or Result
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

globalPluginMaps :: IORef PluginMaps
globalPluginMaps = unsafePerformIO $ newIORef (Map.fromList [], Map.fromList [])
{-# NOINLINE globalPluginMaps #-}

-- Yes...this is not transparent. This is necessary to deserialize
-- Actions and Results defined in Plugins. There are workarounds for
-- most cases but to receive an Action in a Cloud Haskell 'expect' we must be
-- able to deserialize in a pure Binary getter.
readPluginMaps :: PluginMaps
readPluginMaps = unsafePerformIO $ readIORef globalPluginMaps
{-# NOINLINE readPluginMaps #-}

withActionKS :: (MonadLevelDB m) => m a -> m a
withActionKS = withKeySpace KS.actions

decodeResult' :: ResultMap -> Wrapped -> Result
decodeResult' pluginMap wrapped =
    case Map.lookup (wrapped^.typeName) pluginMap of
        Just f -> case f wrapped of
            Right act -> act
            Left (ParseFail s) -> error $ "Error deserializing wrapper: " ++ s
            Left _ -> error "Unknown error deserializing wrapper"
        Nothing -> error $ "Type Name: " ++ BS.unpack (wrapped^.typeName)
                    ++ " not matched! Is your plugin registered?"
