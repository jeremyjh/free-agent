{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Core.Action
    ( toAction
    , extractAction, extractResult
    , matchAction, matchResult
    , resultNow
    , registerAction, actionType
    , registerPluginMaps
    , tryExec, tryExecWith
    , tryExecET, tryExecWithET
    , wrap, unWrap
    )
where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses hiding ((.=))

import           Control.Monad.Writer        (tell)
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary
import           Data.Dynamic                (cast)
import           Data.IORef.Lifted
import qualified Data.Map                    as Map
import           System.IO.Unsafe            (unsafePerformIO)

import           Control.Error (hoistEither, hush)
import           Data.SafeCopy
import           Data.Serialize (runGet)

import Data.Aeson (Value(..), fromJSON, (.=), (.:))
import qualified Data.Aeson  as Aeson


-- Serialization instances for Action are all this module as they require specialized
-- and sensitive functions we want to keep encapsulated e.g. (readPluginMaps)

deriveSerializers ''Result
deriveSerializers ''FailResult

instance Stashable FailResult where
    key (FailResult _ key') = key'

-- ActionEnvelope
-- | A general type for transit of Actions through an agent network which
-- may not have the required plugins available to deserialize the
-- wrapped value.
data ActionEnvelope = ActionEnvelope
    { envelopeWrapped :: Wrapped
    , envelopeMeta    :: Aeson.Object
    } deriving (Show, Eq, Typeable, Generic)

instance Stashable ActionEnvelope where
    key = key . envelopeWrapped

instance Runnable ActionEnvelope where
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

extractAction :: Typeable a=> Action -> Maybe a
extractAction (Action a)= cast a

extractResult :: Portable a => Result -> Maybe a
extractResult = hush . unWrap . resultWrapped

matchAction :: Typeable a => (a -> Bool) -> Action -> Bool
matchAction f (Action a) = maybe False f (cast a)

matchResult :: Portable a => (a -> Bool) -> Result -> Bool
matchResult f = maybe False f . extractResult

seal :: Runnable action => action -> ActionEnvelope
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

instance Runnable Action where
    exec (Action action') = exec action'
    execWith (Action action') = execWith action'

instance Stashable Result where
    key = key . resultWrapped

-- | Wrap a concrete action in existential unless it is already an Action
toAction :: (Runnable a) => a -> Action
toAction act = fromMaybe (Action act) (cast act)

resultNow :: (MonadIO io, Runnable action, Portable result)
          => result -> Text -> action -> io Result
resultNow result text' action = do
    time <- getCurrentTime
    return (Result (wrap result) time text' (toAction action))

-- | Use to register your Action types so they can be
-- deserialized dynamically at runtime; invoke as:
registerAction :: forall a. (Runnable a)
               => Proxy a -> ActionsWriter
registerAction  action' = tell [
    ActionUnwrappers (proxyFqName action')
                     (unwrapAction (unWrap :: Unwrapper a))
                     (unwrapJsonAction (unWrapJson :: JsonUnwrapper a))
                     (proxyFqName (Proxy :: Proxy (RunnableResult a)))
                     (unwrapResult (unWrap :: Unwrapper (RunnableResult a)))

    ]

unwrapResult :: Portable a => Unwrapper a -> Result -> Result
unwrapResult uw result =
    case extractResult result of
         Just wrapped ->
             case uw wrapped of
                 Right unwrapped ->result {resultWrapped = wrap unwrapped}
                 _ -> result
         Nothing -> result

-- | Used only to fix the type passed to 'register' - this should not
-- ever be evaluated and will throw an error if it is
actionType :: (Runnable a) => Proxy a
actionType = Proxy :: Proxy a

-- | Does tryAny on exec and converts SomeException to RunnableFail.
tryExec :: (Runnable action, MonadAgent agent)
          => action -> agent (Either RunnableFail Result)
tryExec action' =
    either (Left . convert) id <$> tryAny (exec action')

-- | Does tryAny on execWith and converts SomeException to RunnableFail.
tryExecWith :: (Runnable action, MonadAgent agent)
          => action -> Result -> agent (Either RunnableFail Result)
tryExecWith action' result' =
    either (Left . convert) id <$> tryAny (execWith action' result')

-- | Like tryExec but hoists into an ExceptT monad.
tryExecET :: (Runnable action, MonadAgent agent)
          => action -> ExceptT RunnableFail agent Result
tryExecET = tryExec >=> hoistEither

-- | Like tryExecWith but hoists into an ExceptT monad.
tryExecWithET :: (Runnable action, MonadAgent agent)
          => action -> Result -> ExceptT RunnableFail agent Result
tryExecWithET action' = tryExecWith action' >=> hoistEither

-- | Unwrap a concrete type into an Action
--
unwrapAction :: (Runnable a)
             => Unwrapper a -> Wrapped -> FetchAction
unwrapAction uw wrapped = Action <$> uw wrapped

unwrapJsonAction :: (Runnable a)
             => JsonUnwrapper a -> Value -> FetchAction
unwrapJsonAction uw wrapped = Action <$> uw wrapped

-- Wrap a concrete type for stash or send where it
-- will be decoded to an Action or Result
wrap :: (Portable a) => a -> Wrapped
wrap st = WrappedExists (fqName st) st

-- | Unwrap a 'Wrapper' into a (known) concrete type
unWrap :: forall a. (Portable a) => Wrapped -> Either String a
unWrap (WrappedEncoded _ _ bytes')= safeDecode bytes'
unWrap (WrappedJson _ _ val')=
  case fromJSON val' of
      Aeson.Success val -> Right val
      Aeson.Error reason -> Left reason
unWrap (WrappedExists _ payload) = maybe (Left failcast) Right (cast payload)
    where failcast = convert $ fqName payload <> " does not match " <> fqName (undefined :: a)

safeDecode :: (SafeCopy a) => ByteString -> Either String a
safeDecode = runGet safeGet

unWrapJson :: FromJSON a => Value -> Either String a
unWrapJson jwrapped = case fromJSON jwrapped of
    Aeson.Error msg -> Left msg
    Aeson.Success value' -> Right value'

--used in serialization instances
decodeAction :: UnwrappersMap -> Wrapped -> Maybe Action
decodeAction pluginMap wrapped =
    case Map.lookup ("Action:" ++ wrappedTypeName wrapped) pluginMap of
        Just uwMap -> hush $ actionUnwrapper uwMap wrapped
        Nothing -> Nothing

decodeJsonAction :: UnwrappersMap -> Text -> Value -> Action
decodeJsonAction pluginMap type' action' =
    case Map.lookup ("Action:" ++ type') pluginMap of
        Just uwMap -> case actionJsonUnwrapper uwMap action' of
            Right act -> act
            Left s -> error $ "Error deserializing wrapper: " ++ s
        Nothing -> error $ "Type Name: " ++ convert type'

-- | Set or re-set the top-level Action Map (done after plugins are registered)
registerPluginMaps :: (MonadBase IO m) => UnwrappersMap -> m ()
registerPluginMaps = writeIORef globalPluginMaps

globalPluginMaps :: IORef UnwrappersMap
globalPluginMaps = unsafePerformIO $ newIORef mempty
{-# NOINLINE globalPluginMaps #-}

--TODO: We can dispense with unsafePerformIO!
--Using the WrappedHandler under an Action/Result means we don't unwrap
--when we 'expect' but rather when we 'exec' - last barrier was inside
--ExecState but there we can unwrap inside an AcidState Update
--if we pass a newtyped PluginMap with a stubbed SafeCopy instance
--this lets us unwrap all actions just once whenever the Server restarts
readPluginMaps :: UnwrappersMap
readPluginMaps = unsafePerformIO $ readIORef globalPluginMaps
{-# NOINLINE readPluginMaps #-}

deriveSerializers ''ActionEnvelope
