{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.RunnerSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import qualified Dash.Proto.Runnable.NagiosCommand as NC
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

checkTCP = NC.NagiosCommand { NC.command = "/usr/lib/nagios/plugins/check_tcp"
                  , NC.host = "jeremyhuffman.com"
                  , NC.port = Just 80 }

checkUDP  = checkTCP {NC.command = "/usr/lib/nagios/plugins/check_udp"}
