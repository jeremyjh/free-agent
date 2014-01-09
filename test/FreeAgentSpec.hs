{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgentSpec (main, spec) where

import           AgentPrelude
import           Test.Hspec
import           FreeAgent

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "FreeAgent" $ do
        it "goes out in the world and does a thing" $
            True `shouldBe` True
