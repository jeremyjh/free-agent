{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dash.Plugins
    ( fetchAction, scanActions, decodeAction
    , register, actionType
    , registerAll
    )
where

import           Dash.Prelude
import qualified Prelude                 as P
import           Dash.Store
import           Dash.Lenses
import           Dash.Core
import           Data.Serialize          ( encode, decode)
import qualified Data.Serialize          as Cereal
import           Data.SafeCopy

import qualified Data.ByteString.Char8   as BS
import           Data.Typeable
import qualified Data.Map                as Map

import           Control.Monad.Writer    (runWriter, tell)

-- we need these Orphan instances here because the instance
-- for Serialize of Action has to use wrap function
-- if we left these in Types, wrap would have to be exported
-- and Types would have to import Plugins & then we cycle & boot file
instance Cereal.Serialize Action where
    put (Action a) = Cereal.put $ wrap a
    get = error "decode/get directly on Action can't happen; use decodeAction"

instance SafeCopy Action where
    putCopy (Action a) = putCopy a

instance Eq Action where
    a == b = encode a == encode b

instance Stashable Action where
    key (Action a) = key a

-- | Like Store.Fetch for an action using 'decodeAction' to deserialize
-- 'Wrapper' types using the 'register' 'actionType'
fetchAction :: (MonadLevelDB m, ConfigReader m)
            => Key -> m FetchAction
fetchAction k = do
    pm <- viewConfig plugins
    wrapped <- get k
    return $ case wrapped of
        Nothing -> Left $ NotFound (showStr k)
        Just bs -> decodeAction pm bs

-- | All keys from this keyspace are actions
scanActions :: (MonadLevelDB m, ConfigReader m)
             => Key -> m [FetchAction]
scanActions prefix = do
    pm <- viewConfig plugins
    let decoder = decodeAction pm
    scan prefix queryList { scanMap = decoder . snd }

-- | Deserializes and unWraps the underlying type using
-- the registered 'actionType'for it
decodeAction :: PluginMap -> ByteString -> FetchAction
decodeAction pluginMap bs = do
    wrapper <- case decode bs of
                   Right w -> Right w
                   Left s -> Left $ ParseFail s
    case Map.lookup (wrapper^.typeName) pluginMap of
        Just f -> f wrapper
        Nothing -> error $ "Type Name: " ++ BS.unpack (wrapper^.typeName)
                    ++ " not matched! Is your plugin registered?"

-- | Use to register your Action types so they can be
-- deserialized dynamically at runtime; invoke as:
--
-- > register (actiontype :: MyType)
register :: (Stashable a, Runnable a)
         => a -> PluginWriter
register act = tell [(fqName act, unWrapAction (anUnWrap act))]
  where
      -- This fixes the type for unWrap to be that of the top-level param
      -- This way we get the concrete decode method we need to deserialize
      -- And yet, we bury it in the Existential Action
      anUnWrap :: (Stashable a, Runnable a) => a -> UnWrapper a
      anUnWrap _ = unWrap

-- | Combine the results of multiple PluginWriters from different plugins
registerAll :: PluginWriter -> PluginMap
registerAll = Map.fromList . snd . runWriter

-- | Used only to fix the type passed to 'register' - this should not
-- ever be evaluated and will throw an error if it is
actionType :: (Stashable a, Runnable a) => a
actionType = error "actionType should never be evaluated! Only pass it \
                   \ to register which takes the TypeRep but does not evaluate it."

wrap :: (Storeable a) => a -> Wrapped
wrap st = Wrapped (fqName st) (encode st)

-- | Unwrap a concrete type into an Action
--
unWrapAction :: (Stashable a, Runnable a) =>
                UnWrapper a -> Wrapped -> FetchAction
unWrapAction f wrapped = fmap Action (f wrapped)

-- | Unwrap a 'Wrapper' into a (known) concrete type
unWrap :: (Stashable a) => Wrapped -> Either FetchFail a
unWrap = decodeStore . _wrappedValue

fqName :: (Typeable a) => a -> ByteString
fqName typee =  modName ++ "." ++ name
  where
    name = BS.pack $ P.show $ typeOf typee
    modName = BS.pack $ tyConModule $ typeRepTyCon $ typeOf typee
