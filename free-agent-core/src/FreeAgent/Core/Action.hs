{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Core.Action
    ( toAction
    , resultNow
    , register, actionType
    , registerPluginMaps
    , tryExec, tryExecWith
    , tryExecET, tryExecWithET
    )
where

import           AgentPrelude
import           FreeAgent.Core.Internal.Lenses hiding ((.=))

import           Control.Monad.Writer        (tell)
import           Control.Monad (mzero)
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary
import           Data.Dynamic                (cast)
import qualified Data.Map                    as Map
import qualified Prelude                     as P
import           System.IO.Unsafe            (unsafePerformIO)

import           Control.Error (hoistEither)
import           Data.SafeCopy
import           Data.Serialize (runPut, runGet)

import Data.Aeson (Value(..), fromJSON, (.=), (.:))
import qualified Data.Aeson  as Aeson


-- Serialization instances for Action are all this module as they require specialized
-- and sensitive functions we want to keep encapsulated e.g. (readPluginMaps)

deriveSerializers ''ResultSummary

instance Stashable ResultSummary where
    key (ResultSummary time _ action') = key action' ++ ":" ++ tshow time

deriveSerializers ''FailResult

instance Extractable ResultSummary

instance Resulting ResultSummary where
    summary summ = summ

instance Stashable FailResult where
    key (FailResult _ summ) = key summ

instance Extractable FailResult

instance Resulting FailResult where
    summary (FailResult _ summ) = summ

-- ActionEnvelope
-- | A general type for transit of Actions through an agent network which
-- may not have the required plugins available to deserialize the
-- wrapped value.
data ActionEnvelope = ActionEnvelope
    { envelopeWrapped :: Wrapped
    , envelopeMeta    :: Aeson.Object
    } deriving (Show, Eq, Typeable, Generic)

instance Stashable ActionEnvelope where
    key = wrappedKey . envelopeWrapped
instance Extractable ActionEnvelope
instance Runnable ActionEnvelope Result where
    exec (ActionEnvelope wrapped _) =
        case decodeAction readPluginMaps wrapped of
            Just action' -> exec action'
            Nothing -> return $ Left DeserializationFailure

instance Binary Action where
    put (Action action) = Binary.put (seal action)
    get = unseal Binary.get

instance SafeCopy Action where
    version = 1
    kind = base
    errorTypeName _ = "FreeAgent.Type.Action"
    putCopy (Action action) = contain (safePut $ seal action)
    getCopy = contain $ unseal safeGet

seal :: Runnable action b => action -> ActionEnvelope
seal action = ActionEnvelope
                { envelopeWrapped = wrap action
                , envelopeMeta = mempty --TODO:implement meta
                }

unseal :: Monad m => m ActionEnvelope -> m Action
unseal mget =
     do env@(ActionEnvelope wrapped _) <- mget
        let pm = unsafePerformIO $ readIORef globalPluginMaps
        case decodeAction pm wrapped of
            Just action' -> return action'
            Nothing -> return (Action env)

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

instance Extractable Result where
    extract (Result a) = cast a

instance Resulting Result where
    summary (Result a) = summary a
    matchR f (Result a)  = maybe False f (cast a)

instance Extractable Action where
    extract (Action a) = cast a

instance Runnable Action Result where
    exec (Action action') = do
        execR <- exec action'
        return $ flip fmap execR $ \result ->
                Result result

    execWith (Action action') result' = do
        execR <- execWith action' result'
        return $ flip fmap execR $ \result ->
                Result result

    matchA f (Action a)  = maybe False f (cast a)

instance Stashable Result where
    key (Result a) = key a

-- | Wrap a concrete action in existential unless it is already an Action
toAction :: (Runnable a b) => a -> Action
toAction act = fromMaybe (Action act) (cast act)

resultNow :: (MonadIO io, Runnable action b)
          => Text -> action -> io ResultSummary
resultNow text' action = do
    time <- getCurrentTime
    return (ResultSummary time text' (toAction action))

-- | Use to register your Action types so they can be
-- deserialized dynamically at runtime; invoke as:
--
-- > register (actiontype :: MyType)
register :: forall a b. (Runnable a b, Resulting b)
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
actionType :: (Runnable a b) => a
actionType = error "actionType should never be evaluated! Only pass it \
                   \ to register which takes the TypeRep but does not evaluate it."

-- | Does tryAny on exec and converts SomeException to RunnableFail.
tryExec :: (Runnable action result, MonadAgent agent)
          => action -> agent (Either RunnableFail result)
tryExec action' =
    tryAny (exec action') >>= return . either (Left . convert) id

-- | Does tryAny on execWith and converts SomeException to RunnableFail.
tryExecWith :: (Runnable action result, MonadAgent agent)
          => action -> Result -> agent (Either RunnableFail result)
tryExecWith action' result' =
    tryAny (execWith action' result') >>= return . either (Left . convert) id

-- | Like tryExec but hoists into an EitherT monad.
tryExecET :: (Runnable action result, MonadAgent agent)
          => action -> EitherT RunnableFail agent result
tryExecET = tryExec >=> hoistEither

-- | Like tryExecWith but hoists into an EitherT monad.
tryExecWithET :: (Runnable action result, MonadAgent agent)
          => action -> Result -> EitherT RunnableFail agent result
tryExecWithET action' = tryExecWith action' >=> hoistEither

-- | Unwrap a concrete type into an Action
--
unwrapAction :: (Runnable a b)
             => Unwrapper a -> Wrapped -> FetchAction
unwrapAction uw wrapped = Action <$> uw wrapped

unwrapJsonAction :: (Runnable a b)
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
wrap st = Wrapped (key st) (fqName st) (safeEncode st)

-- | Unwrap a 'Wrapper' into a (known) concrete type
unWrap :: (Stashable a) => Wrapped -> Either String a
unWrap = safeDecode . wrappedValue

safeEncode :: (SafeCopy a) => a -> ByteString
safeEncode = runPut . safePut

safeDecode :: (SafeCopy a) => ByteString -> Either String a
safeDecode = runGet safeGet

unWrapJson :: FromJSON a => Value -> Either String a
unWrapJson jwrapped = case fromJSON jwrapped of
    Aeson.Error msg -> Left msg
    Aeson.Success value' -> Right value'

--used in serialization instances
decodeAction :: UnwrappersMap -> Wrapped -> Maybe Action
decodeAction pluginMap wrapped@(Wrapped _ type' _) =
    case Map.lookup ("Action:" ++ type') pluginMap of
        Just uwMap ->
            case actionUnwrapper uwMap wrapped of
                Right act -> Just act
                Left _ -> Nothing
        Nothing -> Nothing

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
-- able to deserialize in a pure Binary getter. This also makes persistence
-- of existentials in AcidState a lot cleaner in the client code.
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
deriveSerializers ''ActionEnvelope
