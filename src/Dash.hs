{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash
    ( module Dash.Prelude
    , module Dash.Types
    , dashMain
    ) where


import           Dash.Prelude
import           Dash.Store
import           Dash.Types


dashMain :: AgentConfig -> IO ()
dashMain _ =  do
    runCreateLevelDB "/tmp/leveltest10" "hello" $ do
        put "first" "firstvalue"
        put "second" "secondvalue"

    return ()

