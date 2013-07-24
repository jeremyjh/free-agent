{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dash.Plugins.Nagios(NC.Command(..), registerUnWrappers) where

import           BasicPrelude
import qualified Prelude                           as P
import qualified Dash.Plugins.Nagios.Proto.Command as NC
import           {-# SOURCE #-}
                 Dash.Action                       (Action(..))
import           Dash.Proto
import           Dash.Store                        (Stashable(..), Key(..))
import           Dash.Runner                       (Runnable(..), RunStatus(..))
import           System.Process                    (readProcess)

registerUnWrappers :: [(Utf8, Wrapper -> Action a)]
registerUnWrappers = [ (".dash.plugins.nagios.proto.Command",
                         (\w -> Action $ (unWrap :: Wrapper -> NC.Command) w ))
                     ]

instance ProtoBuf NC.Command
instance Stashable NC.Command where
    key cmd = Key $ toStrict $ utf8 $ NC.host cmd

instance Runnable NC.Command where
    exec cmd =
        readProcess (uToString $ NC.command cmd) (makeArgs cmd) []
            >> return (Complete $ Just "Awesome")
      where
        makeArgs c = ["-H", uToString $ NC.host c, "-p", port $ NC.port c]
        port (Just p) = P.show p
        port Nothing = ""
