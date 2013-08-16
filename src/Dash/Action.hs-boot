{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ExistentialQuantification #-}

module Dash.Action(Action(..), unWrapAction) where

import           BasicPrelude
import           Dash.Proto                       (Wrapper(..), ProtoFail)
import           Dash.Store                       (Stashable(..))
import           Dash.Runner                      (Runnable(..))

data Action a  = forall p. (Stashable p, Runnable p, Typeable p) => Action p

unWrapAction :: (Stashable a, Runnable a) =>
                (Wrapper -> Either ProtoFail a) -> Wrapper -> Either ProtoFail (Action b)
