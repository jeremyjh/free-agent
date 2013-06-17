module Dash.Runner.NagCmdSpec (main, spec) where

import           Test.Hspec
import           Dash.Runner.NagCmd
import           Dash.Runner

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "dash runner nagcmd" $
    it "runs a shell script and returns the text" $
      exec RunNagCmd{checkCommand="./thing.sh"}
      >>= shouldBe (Complete $ Just "Awesome")
