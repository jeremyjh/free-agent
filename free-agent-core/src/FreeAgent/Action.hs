{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Action
    ( toAction
    , register, actionType
    , registerPluginMaps
    )
where

import           AgentPrelude
import           FreeAgent.Lenses

import           Control.Monad.Writer        (tell)
import Control.Monad (mzero)
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary
import           Data.Dynamic                (cast)
import qualified Data.Map                    as Map
import qualified Prelude                     as P
import           System.IO.Unsafe            (unsafePerformIO)

import           Data.SafeCopy
import           Data.Serialize              (Serialize)
import qualified Data.Serialize              as Cereal

import           Data.Aeson (Value(..), fromJSON, (.=), (.:))
import qualified Data.Aeson  as Aeson

-- Serialization instances for Action are all this module as they require specialized
-- and sensitive functions we want to keep encapsulated e.g. (readPluginMaps)

deriving instance Eq ResultSummary
deriveSerializers ''ResultSummary

instance Binary Action where
    put (Action a) = Binary.put $ wrap a
    get = do
        wrapped <- Binary.get
        return $ decodeAction' readPluginMaps wrapped

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
        return $ decodeAction' readPluginMaps wrapped

instance Stashable Action where
    key (Action a) = key a

instance FromJSON Action where
    parseJSON (Object value') = do
        type'  <- value' .: "type"
        action' <- value' .: "action"
        return $ decodeJsonAction readPluginMaps type' action'
    parseJSON _ = mzero

instance ToJSON Action where
    toJSON (Action action') =
        Aeson.object ["type" .= fqName action', "action" .= toJSON action']

-- same as Action - we need Result serialization instances here
instance Binary Result where
    put (Result a) = Binary.put $ wrap a
    get = do
        wrapped <- Binary.get
        return $ decodeResult' readPluginMaps wrapped

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
        return $ decodeResult' readPluginMaps wrapped

instance FromJSON Result where
    parseJSON (Object value') = do
        type'  <- value' .: "type"
        result' <- value' .: "result"
        return $ decodeJsonResult readPluginMaps type' result'
    parseJSON _ = mzero

instance ToJSON Result where
    toJSON (Result result') =
        Aeson.object ["type" .= fqName result', "result" .= toJSON result']


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

-- | Wrap a concrete action in existential unless it is already an Action
toAction :: (Actionable a b) => a -> Action
toAction act = fromMaybe (Action act) (cast act)

-- | Use to register your Action types so they can be
-- deserialized dynamically at runtime; invoke as:
--
-- > register (actiontype :: MyType)
register :: forall a b. (Actionable a b, Resulting b)
         => a -> ActionsWriter
register action' = tell [
    ActionUnwrappers (fqName action')
                     (unwrapAction (unWrap :: Unwrapper a))
                     (unwrapJsonAction (unWrapJson :: JsonUnwrapper a))
                     (fqName (undefined :: b))
                     (unwrapResult (unWrap :: Unwrapper b))
                     (unwrapJsonResult (unWrapJson :: JsonUnwrapper b))
    ]

-- | Used only to fix the type passed to 'register' - this should not
-- ever be evaluated and will throw an error if it is
actionType :: (Actionable a b) => a
actionType = error "actionType should never be evaluated! Only pass it \
                   \ to register which takes the TypeRep but does not evaluate it."

-- | Unwrap a concrete type into an Action
--
unwrapAction :: (Actionable a b)
             => Unwrapper a -> Wrapped -> FetchAction
unwrapAction uw wrapped = Action <$> uw wrapped

unwrapJsonAction :: (Actionable a b)
             => JsonUnwrapper a -> Value -> FetchAction
unwrapJsonAction uw wrapped = Action <$> uw wrapped

unwrapResult :: (Resulting b)
             => Unwrapper b -> Wrapped -> Either String Result
unwrapResult uw wrapped = Result <$> uw wrapped

unwrapJsonResult :: (Resulting b)
             => JsonUnwrapper b -> Value -> Either String Result
unwrapJsonResult uw wrapped = Result <$> uw wrapped

-- Wrap a concrete type for stash or send where it
-- will be decoded to an Action or Result
wrap :: (Stashable a) => a -> Wrapped
wrap st = Wrapped (key st) (fqName st) (Cereal.encode st)

-- | Unwrap a 'Wrapper' into a (known) concrete type
unWrap :: (Stashable a) => Wrapped -> Either String a
unWrap = Cereal.decode . wrappedValue

unWrapJson :: FromJSON a => Value -> Either String a
unWrapJson jwrapped = case fromJSON jwrapped of
    Aeson.Error msg -> Left msg
    Aeson.Success value' -> Right value'

--used in serialization instances - throws an exception since Binary decode is no maybe
decodeAction' :: UnwrappersMap -> Wrapped -> Action
decodeAction' pluginMap wrapped@(Wrapped _ type' _) =
    case Map.lookup ("Action:" ++ type') pluginMap of
        Just uwMap -> case actionUnwrapper uwMap wrapped of
            Right act -> act
            Left s -> error $ "Error deserializing wrapper: " ++ s
        Nothing -> error $ "Type Name: " ++ convert type'
                    ++ " not matched! Is your plugin registered?"

decodeJsonAction :: UnwrappersMap -> Text -> Value -> Action
decodeJsonAction pluginMap type' action' =
    case Map.lookup ("Action:" ++ type') pluginMap of
        Just uwMap -> case actionJsonUnwrapper uwMap action' of
            Right act -> act
            Left s -> error $ "Error deserializing wrapper: " ++ s
        Nothing -> error $ "Type Name: " ++ convert type'
                    ++ " not matched! Is your plugin registered?"

-- | Set or re-set the top-level Action Map (done after plugins are registered)
registerPluginMaps :: (MonadBase IO m) => UnwrappersMap -> m ()
registerPluginMaps = writeIORef globalPluginMaps

globalPluginMaps :: IORef UnwrappersMap
globalPluginMaps = unsafePerformIO $ newIORef (Map.fromList [])
{-# NOINLINE globalPluginMaps #-}

-- Yes...this is not transparent. This is necessary to deserialize
-- Actions and Results defined in Plugins. There are workarounds for
-- most cases but to receive an Action in a Cloud Haskell 'expect' we must be
-- able to deserialize in a pure Binary getter.
readPluginMaps :: UnwrappersMap
readPluginMaps = unsafePerformIO $ readIORef globalPluginMaps
{-# NOINLINE readPluginMaps #-}

decodeResult' :: UnwrappersMap -> Wrapped -> Result
decodeResult' pluginMap wrapped@(Wrapped _ type' _) =
    case Map.lookup ("Result:" ++ type') pluginMap of
        Just uwMap -> case resultUnwrapper uwMap wrapped of
            Right act -> act
            Left s -> error $ "Error deserializing wrapper: " ++ s
        Nothing -> error $ "Type Name: " ++ convert type'
                    ++ " not matched! Is your plugin registered?"

--used in serialization instances - throws an exception since Binary decode is no maybe
decodeJsonResult :: UnwrappersMap -> Text -> Value -> Result
decodeJsonResult pluginMap type' value' =
    case Map.lookup ("Result:" ++ type') pluginMap of
        Just uwMap -> case resultJsonUnwrapper uwMap value' of
            Right act -> act
            Left s -> error $ "Error deserializing wrapper: " ++ s
        Nothing -> error $ "Type Name: " ++ convert type'
                    ++ " not matched! Is your plugin registered?"
