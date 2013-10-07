{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Dash.Action (Action(..), unWrapAction, fetchAction) where

import           Dash.Prelude
import qualified Prelude as P
import           Data.Typeable  (mkTyConApp, mkTyCon3, TypeRep)
import           Dash.Store     hiding (get, put)
import qualified Dash.Store     as DB
import           Data.Serialize
import           Dash.Runner    (Runnable(..))
import           Dash.Plugins   (pluginUnWrapper)


data Action a = forall p. (Stashable p, Runnable p, Typeable p) => Action p

instance Typeable (Action a) where
    typeOf _ = actionType

instance Eq (Action a) where
    a == b = encode a == encode b

instance P.Show (Action a) where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance Serialize (Action a) where
    put (Action a) = put a
    get = error "decode/get directly on Action can't happen; use decodeAction"

instance Stashable (Action a) where
    key (Action s) = key s

instance Runnable (Action a) where
    exec (Action s) = exec s

actionType :: TypeRep
actionType  =  mkTyConApp (mkTyCon3 "dash" "Dash.Action" "Action") []

decodeAction :: ByteString -> Either StashFail (Action a)
decodeAction bs = cerResult (decode bs) >>= pluginUnWrapper
  where
    cerResult (Left s) = Left $ ParseFail s
    cerResult (Right w) = Right w

-- | Useful for plugins registerUnWrappers to simplify the unwrapper function
--
-- e.g. unWrapAction (unWrap :: Wrapper -> NC.Command)
unWrapAction :: (Stashable a, Runnable a) =>
                (Wrapper ->  Either StashFail a) -> Wrapper -> Either StashFail (Action b)
unWrapAction f wrapper = fmap Action (f wrapper)

-- | Like Store.Fetch for an action using 'decodeAction' to deserialize
--
fetchAction :: Key -> LevelDB (Either StashFail (Action a))
fetchAction k = map decode_found $ DB.get k
  where
    decode_found Nothing = Left $ NotFound (showStr k)
    decode_found (Just bs) = decodeAction bs
