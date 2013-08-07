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
                withDB stash checkTCP >>= shouldReturn (return())
            it "reads Commands from the DB" $
                withDB fetch "jeremyhuffman.com" >>= shouldBe (Right checkTCP)
            it "writes wrapped Commands to the DB" $
                withDB stashWrapped checkTCP >>= shouldReturn (return())
            it "reads Commands from the DB as Action" $
                withDB fetchAction "jeremyhuffman.com" >>= shouldBe (Right $ Action checkTCP)
            it "can read an Action from the DB and execute it" $ do
                (Right action) <- withDB fetchAction "jeremyhuffman.com"
                exec action >>= shouldBe (Complete $ Just "Awesome")
            it "will fail to read if key is wrong" $
                withDB fetchProtoNC "notgonnamatch" >>= shouldBe (Left (NotFound "Key \"notgonnamatch\""))
            it "can write an arbitrary bytestring" $
                withDB2 put "somekey" "somevalue" >>= shouldReturn (return ())
            it "will fail to deserialize if data is not a protobuf" $ do
                (Left (ParseFail msg)) <- withDB fetchProtoNC "somekey"
                take 25 msg `shouldBe` "Failed at 1 : Text.Protoc"
        describe "has a reader context API that" $ do
            it "is awesome" $ do
               (Just simple, Right proto) <- runDB "/tmp/leveltest" "awesome" $ do
                    putR "thekey" "thevalue"
                    simple <- getR "thekey"
                    stashWrappedR checkTCP
                    proto <- fetchR "jeremyhuffman.com"
                    return (simple, join $ map unWrap proto)
               simple `shouldBe` "thevalue"
               proto `shouldBe` checkTCP

fetchProtoNC :: DB -> Key -> ResIO (Either ProtoFail NC.Command)
fetchProtoNC = fetch

withDB2 :: (DB -> a -> b -> ResIO c) -> a -> b -> IO c
withDB2 f a b = runResourceT $ do
    db <- openDB "/tmp/leveltest"
    f db a b

withDB :: (DB -> a -> ResIO b) -> a -> IO b
withDB f a = runResourceT $ do
    db <- openDB "/tmp/leveltest"
    f db a

checkTCP = NC.Command { NC.command = "/usr/lib/nagios/plugins/check_tcp"
                  , NC.host = "jeremyhuffman.com"
                  , NC.port = Just 80 }
