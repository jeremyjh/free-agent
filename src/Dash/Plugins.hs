{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dash.Plugins where

import           BasicPrelude
import qualified Prelude as P
import           Dash.Proto
import           Dash.Store                        (Stashable(..), Key(..))
import qualified Dash.Proto.Runnable.NagiosCommand as NC
import           Dash.Runner                       (Runnable(..), RunStatus(..))
import           System.Process(readProcess)



instance ProtoBuf NC.NagiosCommand
instance Stashable NC.NagiosCommand where
    key cmd = Key $ toStrict $ utf8 $ NC.host cmd

instance Runnable NC.NagiosCommand where
    exec cmd = readProcess (uToString $ NC.command cmd) (makeArgs cmd) []
                 >> return (Complete $ Just "Awesome")
      where
        makeArgs c = ["-H", uToString $ NC.host c, "-p", port $ NC.port c]
        port (Just p) = P.show p
        port Nothing = ""
