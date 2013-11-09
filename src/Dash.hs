{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash
    ( module Dash.Prelude
    , module Dash.Types
    , module Dash.Action
    , module Dash.Core
    , dashMain
    ) where


import           Dash.Prelude
import           Dash.Types
import           Dash.Action
import           Dash.Core
import           Database.LevelDB.Higher


dashMain :: AgentConfig -> IO ()
dashMain _ =  do
    runCreateLevelDB "/tmp/leveltest10" "hello" $ do
        put "first" "firstvalue"
        put "second" "secondvalue"

    return ()

