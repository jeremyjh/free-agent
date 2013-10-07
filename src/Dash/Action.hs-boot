{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ExistentialQuantification #-}

module Dash.Action(Action(..), unWrapAction) where

import           BasicPrelude
import           Dash.Store
import           Dash.Runner                      (Runnable(..))

data Action a  = forall p. (Stashable p, Runnable p, Typeable p) => Action p

unWrapAction :: (Stashable a, Runnable a) =>
                (Wrapper -> Either StashFail a) -> Wrapper -> Either StashFail (Action b)
