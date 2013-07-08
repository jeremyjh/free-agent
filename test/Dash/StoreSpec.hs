{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.StoreSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import           Dash.Proto
import           Dash.Store
import           Control.Monad.Trans.Resource      (release, ResIO, runResourceT)
import qualified Dash.Proto.Runnable.NagiosCommand as NC

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Dash.Store" $ do
        it "writes Commands to the DB" $
             withDB writeCommand checkTCP >>= shouldReturn (return())
        it "reads Commands from the DB" $
            withDB readCommand "jeremyhuffman.com" >>= shouldBe checkTCP
        it "will fail to read if key is wrong" $
            withDB readCommand "notgonnamatch" `shouldThrow` errorCall "Didn't find value for key"
        it "can write an arbitrary bytestring" $
            withDB2 putV "somekey" "somevalue" >>= shouldReturn (return ())
        it "will fail to deserialize if data is not a protobuf" $
            withDB readCommand "somekey" `shouldThrow` anyErrorCall

withDB2 f a b = runResourceT $ do
    db <- openDB "/tmp/leveltest"
    f db a b

withDB f a = runResourceT $ do
    db <- openDB "/tmp/leveltest"
    f db a

checkTCP = NC.NagiosCommand { NC.command = "/usr/lib/nagios/plugins/check_tcp"
                  , NC.host = "jeremyhuffman.com"
                  , NC.port = Just 80 }
