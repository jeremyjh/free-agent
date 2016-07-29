{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Core.Action.CompositionSpec (main, spec) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core
import           FreeAgent.Core.Action.Composition

import           FreeAgent.TestHelper
import           FreeAgent.Fixtures

import qualified Data.Binary as Binary

import Control.Distributed.Process.Extras.Time
       (microsecondsToNominalDiffTime)
import           Test.Hspec
import Data.Time.Clock (diffUTCTime)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "ActionPlan" $
        it "can serialize and deserialize existentially" $ testAgent (
                do let plan = toAction $ planExec checkTCP `thenExec` checkTCP
                       bytes = Binary.encode plan
                   Right unencoded <- decodeComposite $ Binary.decode bytes
                   return unencoded
            ) `shouldReturn` toAction (planExec checkTCP `thenExec` checkTCP) 

    describe "ActionPlan combinators" $ do
        it "can execute some actions sequentially" $ testAgent (
                do Right results <- exec $
                           planExec slowTestAction
                           `thenExec` slowTestAction
                           `thenExec` slowTestAction
                           `thenExec` slowTestAction
                   let Just (ResultList [one, _, _, two]) = extractResult results
                   let diff = diffUTCTime (resultTimestamp two) (resultTimestamp one)
                   return (diff > microsecondsToNominalDiffTime 30000)
            ) `shouldReturn` True

        it "can execute some actions in parallel" $ testAgent (
                 do Right results <- exec $
                            planExec slowTestAction
                            `whileExec` slowTestAction
                            `whileExec` slowTestAction
                            `whileExec` slowTestAction
                    let Just (ResultList [one, _, _, two]) = extractResult results
                    let diff = diffUTCTime (resultTimestamp two) (resultTimestamp one)
                    return (diff < microsecondsToNominalDiffTime 5000)
            ) `shouldReturn` True

        it "can fire another actionplan on failure" $ testAgent (
                do Right results <- exec $
                           planExec (testFailAction "will fail")
                           `onFailure` testFailAction "will recover"

                   return $ resultText results
            ) `shouldReturn` "onFailure called"
