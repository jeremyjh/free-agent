{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module DashSpec (main, spec) where

import           Dash.Prelude
import           Test.Hspec
import           Dash

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Dash" $ do
        it "goes out in the world and does a thing" $
            True `shouldBe` True
