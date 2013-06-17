module Dash.Runner where

data RunStatus = Running (Maybe String) | Complete (Maybe String) | Failed (Maybe String)
  deriving (Show, Eq)

class Runnable a where
  exec :: a -> IO RunStatus
