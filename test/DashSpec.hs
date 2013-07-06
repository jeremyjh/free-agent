{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module DashSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import           Dash
import qualified Dash.Proto.Runnable.NagiosCommand as NC

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "dash" $ do
        it "goes out in the world and does a thing" $
            doThing >>= shouldBe ("Thing gonna wait .5 seconds..."
                               ++ "\nThing done waiting.\n")
        it "writes commands to a file" $
            writeCommand tempFile checkTCP
                >>= shouldReturn (return ())
        it "reads commands from a file" $
            readCommand tempFile
                >>= shouldBe checkTCP

tempFile = "/tmp/jnag.dat"

checkTCP = NC.NagiosCommand { NC.command = "/usr/lib/nagios/plugins/check_tcp"
                  , NC.host = "jeremyhuffman.com"
                  , NC.port = Just 80 }

checkUDP  = checkTCP {NC.command = "/usr/lib/nagios/plugins/check_udp"}
