{-#LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- for IsString Utf8
module Dash.Runner
    ( RunStatus(..)
    , exec
    ) where

import           BasicPrelude
import qualified Prelude as P
import           System.Process(readProcess)

import           Text.ProtocolBuffers.Basic(uToString,uFromString,Utf8)
import qualified Dash.Runnable.NagiosCommand as NC

instance IsString Utf8 where fromString = uFromString

data RunStatus = Running (Maybe String)
               | Complete (Maybe String)
               | Failed (Maybe String)
    deriving (Show, Eq)

class Runnable a where
    exec :: a -> IO RunStatus

instance Runnable NC.NagiosCommand where
    exec cmd = readProcess (uToString $ NC.command cmd) (makeArgs cmd) []
            >> return (Complete $ Just "Awesome")
      where
        makeArgs c = ["-H", (uToString $ NC.host c), "-p", port $ NC.port c]
        port (Just p) = P.show p
        port Nothing = ""
