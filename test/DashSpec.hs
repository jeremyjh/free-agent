{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module DashSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import           Dash
import           Dash.Proto
import qualified Dash.Proto.Runnable.NagiosCommand as NC

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Dash" $ do
        it "goes out in the world and does a thing" $
            doThing >>= shouldBe ("Thing gonna wait .5 seconds..."
                               ++ "\nThing done waiting.\n")
