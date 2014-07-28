{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgentSpec (main, spec) where

import           FreeAgent.AgentPrelude
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "FreeAgent" $ do
        it "goes out in the world and does a thing" $
            True `shouldBe` True
