{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash(dashMain) where


import           Dash.Prelude
import qualified Prelude as P
import           Dash.Store
import           Control.Monad.Reader
import           Control.Concurrent.Chan.Lifted


dashMain :: IO ()
dashMain =  do
    runDashDB "/tmp/leveltest10" "hello" $ do
        put "first" "firstvalue"
        put "second" "secondvalue"
        ask >>= putStrLn . show

    return ()

