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


-- | Wrap and store the 'Stashable' in the database
--
stashWrapped :: (Stashable a) => a -> LevelDB ()
stashWrapped s = put (key s) (encode $ wrap s)

wrap :: (Storeable a) => a -> Wrapper
wrap st = Wrapper (fqName st) (encode st)

-- | Useful for plugins registerUnWrappers to simplify the unwrapper function
--
-- e.g. unWrapAction (unWrap :: Wrapper -> NC.Command)
unWrapAction :: (Stashable a, Runnable a) =>
                (Wrapper ->  Either FetchFail a) -> Wrapper -> Either FetchFail (Action b)
unWrapAction f wrapper = fmap Action (f wrapper)

unWrap :: (Stashable a) => Wrapper -> Either FetchFail a
unWrap = decodeStore . value

fqName :: (Typeable a) => a -> ByteString
fqName typee =  modName ++ "." ++ typeName
  where
    typeName = BS.pack $ P.show $ typeOf typee
    modName = BS.pack $ tyConModule $ typeRepTyCon $ typeOf typee
