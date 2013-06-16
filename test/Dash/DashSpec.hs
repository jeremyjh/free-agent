module Dash.DashSpec (Dash.DashSpec.main, spec) where

import           Test.Hspec
import           Dash

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "dash" $
    it "goes out in the world and does a thing" $
      doThing >>= shouldBe "Thing gonna wait .5 seconds...\nThing done waiting.\n"
