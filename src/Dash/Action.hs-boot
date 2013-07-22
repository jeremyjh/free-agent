{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ExistentialQuantification #-}

module Dash.Action where

import           Dash.Store                       (Stashable(..), Key(..))
import           Dash.Runner                      (Runnable(..))
import           Data.Typeable                    (Typeable(..))

data Action a  = forall p. (Stashable p, Runnable p, Typeable p) => Action p
