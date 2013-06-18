module Dash.Runner(RunNagCmd(..)
                  ,RunStatus(..)
                  ,exec
                  ) where

import System.Process(readProcess)

data RunStatus = Running (Maybe String) | Complete (Maybe String) | Failed (Maybe String)
  deriving (Show, Eq)

class Runnable a where
  exec :: a -> IO RunStatus

{-data RunNagCmd = RunNagCmd {checkName :: String-}
                           {-,checkHost :: String-}
                           {-,checkCommand :: String-}
                           {-,interval :: Int -}
                           {-} deriving (Show)-}

data RunNagCmd = RunNagCmd {checkCommand :: String
                           } deriving (Show)

instance Runnable RunNagCmd where
  exec cmd = readProcess (checkCommand cmd) [] []
           >> return (Complete $ Just "Awesome")
