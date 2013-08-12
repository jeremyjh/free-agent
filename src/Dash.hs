{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash(dashMain) where

import qualified Data.ByteString.Char8             as BS
import           Control.Monad.Trans.Resource      (release, ResIO, runResourceT)

import           Dash.Prelude
import           Dash.Proto
import           Dash.Store
import           Dash.Plugins.Nagios

import           Data.Default
import           Database.LevelDB hiding (get, put)

import           Control.Monad.Reader

dashMain :: IO ()
dashMain =  do
    withDBContext "/tmp/leveltest10" "hello" $ do
        putR "first" "firstvalue"
        putR "second" "secondvalue"

    return ()
  where
