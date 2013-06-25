{-#LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.RunnerSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import           Dash.Runner

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "dash runner nagcmd" $ do
        it "does checkTCP" $
            exec checkTCP
            >>= shouldBe (Complete $ Just "Awesome")
        it "fails to checkUDP (bad args)" $
            exec checkUDP `shouldThrow` anyIOException

checkTCP = NagCmd { checkCmd = "/usr/lib/nagios/plugins/check_tcp"
                  , cmdHost = "jeremyhuffman.com"
                  , cmdPort = 80 }

checkUDP  = checkTCP {checkCmd = "/usr/lib/nagios/plugins/check_udp"}
