{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash(dashMain) where


import           Dash.Prelude
import qualified Prelude as P
import           Dash.Store
import           Dash.Types


dashMain :: AgentConfig -> IO ()
dashMain _ =  do
    runCreateLevelDB "/tmp/leveltest10" "hello" $ do
        put "first" "firstvalue"
        put "second" "secondvalue"

    return ()

