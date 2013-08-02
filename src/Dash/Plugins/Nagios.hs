{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dash.Plugins.Nagios(Command(..), registerUnWrappers) where

import           Dash.Prelude
import           Dash.Plugins.Nagios.Proto.Command
import           {-# SOURCE #-}
                 Dash.Action                       (Action(..), unWrapAction)
import           Dash.Proto
import           Dash.Store                        (Stashable(..), Key(..))
import           Dash.Runner                       (Runnable(..), RunStatus(..))
import           System.Process                    (readProcess)

registerUnWrappers :: [(Utf8, Wrapper -> Either ProtoFail (Action a))]
registerUnWrappers = [ (".dash.plugins.nagios.proto.Command",
                          unWrapAction (unWrap :: Wrapper -> Either ProtoFail Command) )
                     ]


instance ProtoBuf Command
instance Stashable Command where
    key cmd = Key $ utf8S $ host cmd

instance Runnable Command where
    exec cmd =
        readProcess (uToString $ command cmd) (makeArgs cmd) []
            >> return (Complete $ Just "Awesome")
      where
        makeArgs c = ["-H", uToString $ host c, "-p", portS $ port c]
        portS (Just p) = showStr p
        portS Nothing = ""
