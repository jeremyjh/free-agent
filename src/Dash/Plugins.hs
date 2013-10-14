{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Dash.Plugins
    ( fetchAction, decodeAction
    , register, unWrap
    , stashWrapped
    )
where

import           Dash.Prelude
import qualified Prelude                 as P
import           Dash.Store              as DB
import           Dash.Types
import           Data.Serialize          (encode, decode)

import           Data.Typeable
import qualified Data.Map                as Map
import qualified Data.ByteString.Char8   as BS

-- | Like Store.Fetch for an action using 'decodeAction' to deserialize
--
fetchAction :: (MonadLevelDB m) => Key -> Config m FetchAction
fetchAction k = do
    pm <- asks configPlugins
    wrapped <- DB.get k
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
                error $ "Type Name: " ++ BS.unpack typeName ++ " not matched! Is your plugin registered?"

register :: (Stashable a, Runnable a)
         => ByteString -> UnWrapper a -> PluginUnWrapper Action
register bs uw = (bs, unWrapAction uw)

-- | Wrap and store the 'Stashable' in the database
--
stashWrapped :: (Stashable a, MonadLevelDB m) => a -> m ()
stashWrapped s = put (key s) (encode $ wrap s)

wrap :: (Storeable a) => a -> Wrapped
wrap st = Wrapped (fqName st) (encode st)

-- | Useful for plugins registerUnWrappers to simplify the unwrapper function
--
-- e.g. unWrapAction (unWrap :: Wrapper -> NC.Command)
unWrapAction :: (Stashable a, Runnable a) =>
                UnWrapper a -> Wrapped -> FetchAction
unWrapAction f wrapped = fmap Action (f wrapped)

unWrap :: (Stashable a) => Wrapped -> Either FetchFail a
unWrap = decodeStore . value

fqName :: (Typeable a) => a -> ByteString
fqName typee =  modName ++ "." ++ typeName
  where
    typeName = BS.pack $ P.show $ typeOf typee
    modName = BS.pack $ tyConModule $ typeRepTyCon $ typeOf typee
