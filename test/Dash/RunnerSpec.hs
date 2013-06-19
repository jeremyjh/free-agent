{-#LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.RunnerSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import           Dash.Runner

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "dash runner nagcmd" $
        it "runs a shell script and returns the text" $
            exec checkTCP
            >>= shouldBe (Complete $ Just "Awesome")

checkTCP :: NagCmd
checkTCP = NagCmd { checkCmd = "/usr/lib/nagios/plugins/check_tcp"
                  , cmdHost = "jeremyhuffman.com"
                  , cmdPort = 80 }
