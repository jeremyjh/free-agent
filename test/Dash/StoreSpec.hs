{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.StoreSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec

import           Dash.Proto
import           Dash.Store
import           Control.Monad.Trans.Resource      (release, ResIO, runResourceT, liftResourceT)
import qualified Dash.Plugins.Nagios.Proto.Command as NC
import           Dash.Runner
import           Dash.Plugins                      ()
import           Dash.Action

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Dash.Store" $ do
        describe "has a simple API that" $ do
            it "writes Commands to the DB" $
                withDBT (stashR checkTCP) >>= shouldReturn (return())
            it "reads Commands from the DB" $
                withDBT (fetchR "jeremyhuffman.com") >>= shouldBe (Right checkTCP)
            it "writes wrapped Commands to the DB" $
                withDBT (stashWrappedR checkTCP) >>= shouldReturn (return())
            it "reads Commands from the DB as Action" $ do
                action <- withDBT (fetchAction "jeremyhuffman.com")
                action `shouldBe` (Right $ Action checkTCP)
            it "can read an Action from the DB and execute it" $ do
                (Right action) <- withDBT (fetchAction "jeremyhuffman.com")
                exec action >>= shouldBe (Complete $ Just "Awesome")
            it "will fail to read if key is wrong" $ do
                (Left (NotFound _)) <- withDBT (fetchProtoNC "notgonnamatch")
                True `shouldBe` True -- NOT exception
            it "can write an arbitrary bytestring" $
                withDBT (putR "somekey" "somevalue") >>= shouldReturn (return ())
            it "will fail to deserialize if data is not a protobuf" $ do
                (Left (ParseFail msg)) <- withDBT (fetchProtoNC "somekey")
                take 25 msg `shouldBe` "Failed at 1 : Text.Protoc"
        describe "has a reader context API that" $
            it "is awesome" $ do
                (Just simple, Right proto)
                    <- withDBContext testDB "awesome" $ do
                        putR "thekey" "thevalue"
                        withKeySpace "otherspace" $ do
                            putR "thekey" "othervalue"
                        simple <- getR "thekey"
                        stashWrappedR checkTCP
                        proto <- fetchR "jeremyhuffman.com"
                        return (simple, join $ map unWrap proto)
                simple `shouldBe` "thevalue"
                proto `shouldBe` checkTCP

testDB = "/tmp/leveltest"

withDBT :: DBContextIO a -> IO a
withDBT = withDBContext testDB "Dash.StoreSpec"

fetchProtoNC :: Key -> DBContextIO (Either ProtoFail NC.Command)
fetchProtoNC = fetchR

checkTCP = NC.Command { NC.command = "/usr/lib/nagios/plugins/check_tcp"
                  , NC.host = "jeremyhuffman.com"
                  , NC.port = Just 80 }
