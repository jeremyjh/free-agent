{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Core.Action.CompositionSpec (main, spec) where

import           FreeAgent.AgentPrelude
import qualified Prelude as P
import           FreeAgent.Core
import           FreeAgent.Core.Lenses
import           FreeAgent.Core.Action.Composition

import           FreeAgent.TestHelper
import           FreeAgent.Fixtures

import qualified Data.Binary as Binary

import Control.Distributed.Process.Platform.Time
       (microsecondsToNominalDiffTime)
import           Test.Hspec
import Data.Time.Clock (diffUTCTime)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ActionPlan" $ do
        it "can serialize and deserialize existentially" $ do
            testAgent $
                let plan = Action $ planExec checkTCP `thenExec` checkTCP
                    bytes = Binary.encode plan
                in return (Binary.decode bytes)
            `shouldReturn` (Action $ planExec checkTCP `thenExec` checkTCP)

    describe "ActionPlan combinators" $ do

        it "can execute some actions sequentially" $ do
            testAgent $ do
                Right results <- exec $
                    planExec slowTestAction
                    `thenExec` slowTestAction
                    `thenExec` slowTestAction
                    `thenExec` slowTestAction
                let Just (ResultList _ [one, _, _, two]) = extract results
                let diff = diffUTCTime (two^.to summary.timestamp) (one^.to summary.timestamp)
                return (diff > microsecondsToNominalDiffTime 30000)
            `shouldReturn` True

        it "can execute some actions in parallel" $ do
            testAgent $ do
                Right results <- exec $
                    planExec slowTestAction
                    `whileExec` slowTestAction
                    `whileExec` slowTestAction
                    `whileExec` slowTestAction
                let Just (ResultList _ [one, _, _, two]) = extract results
                let diff = diffUTCTime (two^.to summary.timestamp) (one^.to summary.timestamp)
                return (diff < (microsecondsToNominalDiffTime 5000))
            `shouldReturn` True

        it "can fire another actionplan on failure" $ do
            testAgent $ do
                Right results <- exec $
                    planExec (testFailAction "will fail")
                    `onFailure` (testFailAction "will recover")
                let Just (ResultList _ [result']) = extract results
                return $ result'^.to summary.text
            `shouldReturn` "onFailure called"


testAgent ma = quickRunAgent 500
                             ("4121"
                             , appConfig & nodePort .~ "4121"
                             , appPlugins
                             ) ma
