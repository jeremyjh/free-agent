{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Action.ShellCommandSpec (main, spec) where

import           AgentPrelude
import qualified Prelude as P
import           FreeAgent.Lenses
import           FreeAgent.Plugins.Nagios

import           FreeAgent.TestHelper
import           FreeAgent.Fixtures

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ShellCommand - as used by CheckTCP" $ do
        it "'succeeds' on RC 3 - Nagios Critical" $ do
            testAgent $ do
                Right (NagiosResult _ status) <- exec $ checkTCP {_checktcpPort = 632}
                return status
            `shouldReturn` Critical


testAgent ma = quickRunAgent ("4122"
                             , appConfig & nodePort .~ "4122"
                             , appPlugins
                             ) ma
