module Dash.Runner.NagCmd(exec, RunNagCmd(..)) where

import Dash.Runner
import System.Process(readProcess)

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
