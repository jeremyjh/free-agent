{-# LANGUAGE NoImplicitPrelude, OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dash.Plugins.Nagios(Command(..), registerUnWrappers) where

import           Dash.Prelude
import           {-# SOURCE #-}
                 Dash.Action                       (Action(..), unWrapAction)
import           Dash.Store
import           Dash.Runner                       (Runnable(..), RunStatus(..))
import           System.Process                    (readProcess)
import           Data.Serialize                    as Cereal
import           Data.SafeCopy

data Command = Command { host :: Text
               , port :: Maybe Int32
               , command :: Text } deriving (Show, Eq, Typeable)

deriveSafeCopy 1 'base ''Command

instance Serialize Command where
    put = safePut
    get = safeGet

instance Stashable Command where
    key = fromT . host

instance Runnable Command where
    exec cmd =
        readProcess (fromT $ command cmd) (makeArgs cmd) []
            >> return (Complete $ Just "Awesome")
      where
        makeArgs c = ["-H", fromT $ host c, "-p", portS $ port c]
        portS (Just p) = showStr p
        portS Nothing = ""

registerUnWrappers :: [(ByteString, Wrapper -> Either StashFail (Action a))]
registerUnWrappers = [ ("Dash.Plugins.Nagios.Command",
                          unWrapAction (unWrap :: Wrapper -> Either StashFail Command) )
                     ]
