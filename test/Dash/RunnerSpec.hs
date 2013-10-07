{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.RunnerSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import           Dash.Runner
import           Dash.Plugins.Nagios
import           Dash.Action(Action(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "dash runner nagcmd" $ do
        it "does checkTCP" $
            exec checkTCP
                >>= shouldBe (Complete $ Just "Awesome")
        it "does checkTCP in an Action" $
            exec (Action checkTCP)
                >>= shouldBe (Complete $ Just "Awesome")
        it "fails to checkUDP (bad args)" $
            exec checkUDP `shouldThrow` anyIOException

checkTCP = Command { command = "/usr/lib/nagios/plugins/check_tcp"
                  , host = "localhost"
                  , port = Just 17500 }

checkUDP  = checkTCP {command = "/usr/lib/nagios/plugins/check_udp"}
