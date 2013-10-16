{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Dash.Plugins
    ( fetchAction, decodeAction
    , register, actionType, unWrap
    , stashWrapped
    , fqName
    , registerAll
    )
where

import           Dash.Prelude
import qualified Prelude                 as P
import           Dash.Store
import           Dash.Types
import           Data.Serialize          (encode, decode)

import           Data.Typeable
import qualified Data.Map                as Map
import qualified Data.ByteString.Char8   as BS

import           Control.Monad.Writer    (runWriter, tell)

-- | Like Store.Fetch for an action using 'decodeAction' to deserialize
--
fetchAction :: (MonadLevelDB m, ConfigReader m) => Key -> m FetchAction
fetchAction k = do
    pm <- viewConfig plugins
    wrapped <- get k
    return $ case wrapped of
        Nothing -> Left $ NotFound (showStr k)
        Just bs -> decodeAction pm bs

-- | Deserializes and unWraps the underlying type using
-- the 'registerUnWrappers' defined for it
decodeAction :: PluginMap -> ByteString -> FetchAction
decodeAction pluginMap bs =
    case decode bs of
        Right w -> Right w
        Left s -> Left $ ParseFail s
    >>= pluginUnWrapper
  where
    pluginUnWrapper wrapper =
        let typeName = wType wrapper in
        case Map.lookup typeName pluginMap of
            Just f -> f wrapper
            Nothing ->
                error $ "Type Name: " ++ BS.unpack typeName
                        ++ " not matched! Is your plugin registered?"

-- | Use to register your Action types so they can be
-- deserialized dynamically at runtime; invoke as:
--
-- > register (actiontype :: MyType)
register :: (Stashable a, Runnable a)
         => a -> PluginWriter
register act = tell [(fqName act, unWrapAction (anUnWrap act))]
  where
      anUnWrap :: (Stashable a, Runnable a) => a -> UnWrapper a
      anUnWrap _ = unWrap

-- | Combine the results of multiple PluginWriters from different plugins
registerAll :: PluginWriter -> PluginMap
registerAll = fromList . snd . runWriter

actionType :: (Stashable a, Runnable a) => a
actionType = error "actionType should never be evaluated! Only pass it \
                   \ to register which takes the TypeRep but does not evaluate it."

-- | Wrap and store the 'Stashable' in the database
--
stashWrapped :: (Stashable a, MonadLevelDB m) => a -> m ()
stashWrapped s = put (key s) (encode $ wrap s)

wrap :: (Storeable a) => a -> Wrapped
wrap st = Wrapped (fqName st) (encode st)

-- | Unwrap a concrete type into an Action
--
unWrapAction :: (Stashable a, Runnable a) =>
                UnWrapper a -> Wrapped -> FetchAction
unWrapAction f wrapped = fmap Action (f wrapped)

-- | Unwrap a 'Wrapper' into a (known) concrete type
unWrap :: (Stashable a) => Wrapped -> Either FetchFail a
unWrap = decodeStore . value

fqName :: (Typeable a) => a -> ByteString
fqName typee =  modName ++ "." ++ typeName
  where
    typeName = BS.pack $ P.show $ typeOf typee
    modName = BS.pack $ tyConModule $ typeRepTyCon $ typeOf typee
