{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ExistentialQuantification #-}

module Dash.Action (Action(..)) where

import           BasicPrelude
import qualified Prelude as P
import           Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire(..))
import           Text.ProtocolBuffers.Reflections (ReflectDescriptor(..))
import           Dash.Store                       (Stashable(..), Key(..))
import           Dash.Runner                      (Runnable(..))
import           Dash.Proto                       (ProtoBuf(..))

data Action a = forall p. (Stashable p, Runnable p) => Action p

instance Eq (Action a) where
    a == b = encode a == encode b

instance Show (Action a) where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance ReflectDescriptor (Action a) where
    getMessageInfo (Action a) = getMessageInfo a
    reflectDescriptorInfo (Action a) = reflectDescriptorInfo a

instance Wire (Action a) where
    wireSize a (Action s) = wireSize a s
    wirePut a (Action s) = wirePut a s
    wireGet = undefined -- sort of like decode; the return type selects the method

instance ProtoBuf (Action a) where
    encode (Action s) = encode s
    decode = undefined -- we can never decode an Action, only a concrete type

instance Stashable (Action a) where
    key (Action s) = key s

instance Runnable (Action a) where
    exec (Action s) = exec s
