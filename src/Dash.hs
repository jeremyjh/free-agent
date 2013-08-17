{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash(dashMain) where


import           Dash.Prelude
import           Dash.Store


dashMain :: IO ()
dashMain =  do
    runDBContext "/tmp/leveltest10" "hello" $ do
        put "first" "firstvalue"
        put "second" "secondvalue"

    return ()
  where
