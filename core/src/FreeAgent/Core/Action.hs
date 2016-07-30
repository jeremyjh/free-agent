{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts                   #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings           #-}
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies                                                          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeAgent.Core.Action
    ( toAction
    , extractAction, extractResult
    , matchAction, matchResult
    , decodeResult
    , decodeEnvelope
    , resultNow
    , registerAction, actionType
    , tryExec, tryExecWith
    , tryExecET, tryExecWithET
    , wrap, unWrap
    )
where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses hiding ((.=))

import           Control.Monad.Writer           (tell)
import           Data.Binary                    (Binary)
import qualified Data.Binary                    as Binary
import           Data.Dynamic                   (cast)

import qualified Data.Map                       as Map

import           Control.Error                  (hoistEither, hush)
import           Data.SafeCopy
import           Data.Serialize                 (Get, runGet)

import           Data.Aeson                     (Value (..), fromJSON, (.:), (.=))
import qualified Data.Aeson                     as Aeson


-- Serialization instances for Action are all this module as they require specialized
-- and sensitive functions we want to keep encapsulated e.g. (readPluginMaps)

deriveSerializers ''Result
deriveSerializers ''FailResult

-- ActionEnvelope
-- | A general type for transit of Actions through an agent network which
-- may not have the required plugins available to deserialize the
-- wrapped value.
data ActionEnvelope = ActionEnvelope
    { envelopeKey     :: Key
    , envelopeWrapped :: Wrapped
    , envelopeMeta    :: Aeson.Object
    } deriving (Show, Eq, Typeable, Generic)

instance Stashable ActionEnvelope where
    key = envelopeKey

instance Runnable ActionEnvelope where
    exec env =
      do edecoded <- decodeEnvelope $ toAction env
         [qwarn|Executing ActionEnvelope for #{edecoded} |]
         case edecoded of
            Right action' -> exec action'
            Left msg -> return $ Left (DeserializationFailure (convert msg))

instance Binary Action where
    put (Action action) = Binary.put (seal action)
    get = fmap Action (Binary.get :: Binary.Get ActionEnvelope)

instance SafeCopy Action where
    version = 1
    kind = base
    errorTypeName _ = "FreeAgent.Type.Action"
    putCopy (Action action) = contain (safePut $ seal action)
    getCopy = contain $ fmap Action (safeGet :: Get ActionEnvelope)

extractAction :: Typeable a=> Action -> Maybe a
extractAction (Action a)= cast a

extractResult :: Portable a => Result -> Maybe a
extractResult = hush . unWrap . resultWrapped

matchAction :: Typeable a => (a -> Bool) -> Action -> Bool
matchAction f (Action a) = maybe False f (cast a)

matchResult :: Portable a => (a -> Bool) -> Result -> Bool
matchResult f = maybe False f . extractResult

seal :: Runnable action => action -> ActionEnvelope
seal action =
   fromMaybe ActionEnvelope
                { envelopeKey = key action
                , envelopeWrapped = wrap action
                , envelopeMeta = mempty --TODO:implement meta
                }
             (cast action)

instance Stashable Action where
    key (Action a) = key a

instance FromJSON Action where
    parseJSON (Object value') = do
        key' <- value' .: "key"
        type'  <- value' .: "type"
        action'  <- value' .: "action"
        return $ toAction $ ActionEnvelope key' (WrappedJson type' action') mempty
    parseJSON _ = mzero

instance ToJSON Action where
    toJSON (Action action') =
        Aeson.object ["key" .= key action', "type" .= fqName action', "action" .= toJSON action']

instance Runnable Action where
    exec (Action action') = exec action'
    execWith (Action action') = execWith action'

-- | Wrap a concrete action in existential unless it is already an Action
-- in which case the original is returned.
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
    (ActionUnwrappers(proxyFqName action')
                     (unwrapAction (unWrap :: Unwrapper a))
    ,ResultUnwrappers(proxyFqName (Proxy :: Proxy (RunnableResult a)))
                     (unwrapResult (unWrap :: Unwrapper (RunnableResult a)))
    )
    ]

unwrapResult :: Portable a => Unwrapper a -> Result -> Result
unwrapResult uw result =
    let wrapped = resultWrapped result
    in case uw wrapped of
        Right unwrapped ->
            result {resultWrapped = wrap unwrapped}
        Left s ->
            error . convert $
              "unwrapResult failed for: " ++ wrappedTypeName wrapped ++
              " : " ++ convert s

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

-- Wrap a concrete type for stash or send where it
-- will be decoded to an Action or Result
wrap :: (Portable a) => a -> Wrapped
wrap st = WrappedExists (fqName st) st

-- | Unwrap a 'Wrapper' into a (known) concrete type
unWrap :: forall a. (Portable a) => Wrapped -> Either String a
unWrap (WrappedEncoded _ bytes')= safeDecode bytes'
unWrap (WrappedJson _ val')=
  case fromJSON val' of
      Aeson.Success val -> Right val
      Aeson.Error reason -> Left reason
unWrap (WrappedExists _ payload) = maybe (Left failcast) Right (cast payload)
    where failcast = convert $ fqName payload <> " does not match " <> fqName (undefined :: a)

safeDecode :: (SafeCopy a) => ByteString -> Either String a
safeDecode = runGet safeGet

decodeEnvelope :: ContextReader m => Action -> m (Either String Action)
decodeEnvelope action' =
 do pluginMap <- viewContext (plugins.actionUnwrappers)
    return $ case extractAction action' of
        Just envelope ->
            let wrapped = envelopeWrapped envelope
            in case Map.lookup (wrappedTypeName wrapped) pluginMap of
                Just uwMap -> let uw = actionUnwrapper uwMap
                              in uw wrapped
                Nothing -> Left $ "No unwrapper found for: " ++ convert (wrappedTypeName wrapped)
        Nothing -> Right action' --not an envelope - its already decoded

decodeResult :: ContextReader m => Result -> m (Maybe Result)
decodeResult wrapped =
 do pluginMap <- viewContext (plugins.resultUnwrappers)
    return $ case Map.lookup (wrappedTypeName (resultWrapped wrapped)) pluginMap of
                Just uwMap -> Just $ let uw = resultUnwrapper uwMap in uw wrapped
                Nothing -> Nothing

deriveSerializers ''ActionEnvelope
