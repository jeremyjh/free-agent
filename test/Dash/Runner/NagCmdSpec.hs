module Dash.Runner.NagCmdSpec (main, spec) where

import           Test.Hspec
import           Dash.Runner.NagCmd

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "dash runner nagcmd" $
    it "runs a shell script and returns the text" $
      exec >>= shouldBe "Thing gonna wait .5 seconds...\nThing done waiting.\n"
