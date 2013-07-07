{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.StoreSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import           Dash.Proto
import           Dash.Store
import qualified Dash.Proto.Runnable.NagiosCommand as NC

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Dash.Store" $ do
        it "writes Commands to the DB" $
            writeCommand checkTCP >>= shouldReturn (return ())
        it "reads Commands from the DB" $
            readCommand "jeremyhuffman.com" >>= shouldBe checkTCP
        it "will fail to read if key is wrong" $
            readCommand "notgonnamatch" `shouldThrow` errorCall "Didn't find value for key"
        it "can write an arbitrary bytestring" $
            putV "somekey" "somevalue" >>= shouldReturn (return ())
        it "will fail to deserialize if data is not a protobuf" $
            readCommand "somekey" `shouldThrow` anyErrorCall

checkTCP = NC.NagiosCommand { NC.command = "/usr/lib/nagios/plugins/check_tcp"
                  , NC.host = "jeremyhuffman.com"
                  , NC.port = Just 80 }
