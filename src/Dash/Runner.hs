{-#LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.Runner
    ( NagCmd(..)
    , RunStatus(..)
    , exec
    ) where

import           BasicPrelude
import qualified Prelude as P
import           System.Process(readProcess)

data RunStatus = Running (Maybe String)
               | Complete (Maybe String)
               | Failed (Maybe String)
    deriving (Show, Eq)

class Runnable a where
    exec :: a -> IO RunStatus

data NagCmd = NagCmd   { checkCmd :: String
                       , cmdHost :: String
                       , cmdPort :: Integer
                       } deriving (Show)

instance Runnable NagCmd where
    exec cmd = readProcess (checkCmd cmd) (makeArgs cmd) []
            >> return (Complete $ Just "Awesome")
      where
        makeArgs c = ["-H", cmdHost c, "-p", P.show $ cmdPort c]
