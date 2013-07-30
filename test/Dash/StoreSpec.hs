{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.StoreSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import           Dash.Proto
import           Dash.Store
import           Control.Monad.Trans.Resource      (release, ResIO, runResourceT)
import qualified Dash.Plugins.Nagios.Proto.Command as NC
import           Dash.Runner                       (exec)
import           Dash.Plugins                      ()
import           Dash.Action

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Dash.Store" $ do
        it "writes Commands to the DB" $
            withDB stash checkTCP >>= shouldReturn (return())
        it "reads Commands from the DB" $
            withDB fetch "jeremyhuffman.com" >>= shouldBe (Right checkTCP)
        it "reads Commands from the DB as Action" $
            withDB fetchProtoNCRS "jeremyhuffman.com" >>= shouldBe (Right $ Action checkTCP)
        it "will fail to read if key is wrong" $
            withDB fetchProtoNC "notgonnamatch" >>= shouldBe (Left (NotFound "Key \"notgonnamatch\""))
        it "can write an arbitrary bytestring" $
            withDB2 put "somekey" "somevalue" >>= shouldReturn (return ())
        it "will fail to deserialize if data is not a protobuf" $ do
            (Left (ParseFail msg)) <- withDB fetchProtoNC "somekey"
            take 25 msg `shouldBe` "Failed at 1 : Text.Protoc"

fetchProtoNCRS :: DB -> Key -> ResIO (Either ProtoFail (Action a))
fetchProtoNCRS = fetch

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
