{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Dash.Plugins.Common where

import           Dash.Prelude
import qualified Prelude               as P
import           Dash.Types
import           Dash.Store
import           Data.Serialize        (encode)
import qualified Data.ByteString.Char8 as BS
import           Data.Typeable


register :: (Stashable a, Runnable a)
         => ByteString -> UnWrapper a -> PluginUnWrapper (Action b)
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
                (UnWrapper a) -> Wrapped -> FetchAction b
unWrapAction f wrapped = fmap Action (f wrapped)

unWrap :: (Stashable a) => Wrapped -> Either FetchFail a
unWrap = decodeStore . value

fqName :: (Typeable a) => a -> ByteString
fqName typee =  modName ++ "." ++ typeName
  where
    typeName = BS.pack $ P.show $ typeOf typee
    modName = BS.pack $ tyConModule $ typeRepTyCon $ typeOf typee
