{-# LANGUAGE NoImplicitPrelude, OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- for IsString Utf8
module Dash.Runner
    ( RunStatus(..)
    , Runnable(..)
    ) where

import           BasicPrelude

data RunStatus = Running (Maybe String)
               | Complete (Maybe String)
               | Failed (Maybe String)
    deriving (Show, Eq)

class Runnable a where
    exec :: a -> IO RunStatus
