{-#LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.Runner
    ( NagCmd(..)
    , RunStatus(..)
    , exec
    ) where

import           BasicPrelude
import           System.Process(readProcess)

data RunStatus = Running (Maybe String)
               | Complete (Maybe String)
               | Failed (Maybe String)
    deriving (Show, Eq)

class Runnable a where
    exec :: a -> IO RunStatus

{-data RunNagCmd = RunNagCmd {checkName :: String-}
                           {-,checkHost :: String-}
                           {-,checkCommand :: String-}
                           {-,interval :: Int -}
                           {-} deriving (Show)-}

data NagCmd = NagCmd {checkCmd :: String
                     } deriving (Show)

instance Runnable NagCmd where
    exec cmd = readProcess (checkCmd cmd) [] []
            >> return (Complete $ Just "Awesome")
