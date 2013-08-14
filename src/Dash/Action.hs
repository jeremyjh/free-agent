{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ExistentialQuantification #-}

module Dash.Action (Action(..), unWrapAction, fetchAction) where

import           Dash.Prelude
import qualified Prelude as P
import           Data.Typeable  (mkTyConApp, mkTyCon3, TypeRep)
import           Dash.Store     (Stashable(..), Key, fetch, DBContextIO)
import           Dash.Runner    (Runnable(..))
import           Dash.Proto
import           Dash.Plugins   (pluginUnWrapper)


data Action a = forall p. (Stashable p, Runnable p, Typeable p) => Action p

instance Typeable (Action a) where
    typeOf _ = actionType

instance Eq (Action a) where
    a == b = encode a == encode b

instance P.Show (Action a) where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance ReflectDescriptor (Action a) where
    getMessageInfo (Action a) = getMessageInfo a
    reflectDescriptorInfo (Action a) = reflectDescriptorInfo a

instance Wire (Action a) where
    wireSize a (Action s) = wireSize a s
    wirePut a (Action s) = wirePut a s
    wireGet = error "Cannot wireGet on an Action directly"

instance ProtoBuf (Action a) where
    encode = encodeRaw . wrap
    decode bs = decodeRaw bs >>= pluginUnWrapper

instance Stashable (Action a) where
    key (Action s) = key s

instance Runnable (Action a) where
    exec (Action s) = exec s

actionType :: TypeRep
actionType  =  mkTyConApp (mkTyCon3 "dash" "Dash.Action" "Action") []

-- | Useful for plugins registerUnWrappers to simplify the unwrapper function
--
-- e.g. unWrapAction (unWrap :: Wrapper -> NC.Command)
unWrapAction :: (Stashable a, Runnable a) =>
                (Wrapper ->  Either ProtoFail a) -> Wrapper -> Either ProtoFail (Action b)
unWrapAction f wrapper = fmap Action (f wrapper)

-- | Convenience function to fix type to Action a
--
fetchAction :: Key -> DBContextIO (Either ProtoFail (Action a))
fetchAction = fetch
