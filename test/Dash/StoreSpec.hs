{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.StoreSpec (main, spec) where

import           BasicPrelude
import           Test.Hspec
import           Dash.Proto
import           Dash.Store
import           Control.Monad.Trans.Resource      (release, ResIO, runResourceT)
import qualified Dash.Proto.Runnable.NagiosCommand as NC
import           Dash.Runner                       (exec)
import           Dash.Plugins                      ()
import           Dash.Action(Action(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Dash.Store" $ do
        it "writes Commands to the DB" $
            withDB stash checkTCP >>= shouldReturn (return())
        it "reads Commands from the DB" $
            withDB fetch "jeremyhuffman.com" >>= shouldBe checkTCP
        it "reads Commands from the DB as Action" $
            withDB fetchProtoNCRS "jeremyhuffman.com" >>= shouldBe (Action checkTCP)
        it "will fail to read if key is wrong" $
            shouldThrow (withDB fetchProtoNC "notgonnamatch" >>= exec) (errorCall "Didn't find value for key")
        it "can write an arbitrary bytestring" $
            withDB2 put "somekey" "somevalue" >>= shouldReturn (return ())
        it "will fail to deserialize if data is not a protobuf" $ do
            cmd <- withDB fetchProtoNC "somekey"
            exec cmd `shouldThrow` anyException

fetchProtoNCRS :: DB -> Key -> ResIO (Action a)
fetchProtoNCRS = fetch

fetchProtoNC :: DB -> Key -> ResIO NC.NagiosCommand
fetchProtoNC = fetch

withDB2 :: (DB -> a -> b -> ResIO c) -> a -> b -> IO c
withDB2 f a b = runResourceT $ do
    db <- openDB "/tmp/leveltest"
    f db a b

withDB :: (DB -> a -> ResIO b) -> a -> IO b
withDB f a = runResourceT $ do
    db <- openDB "/tmp/leveltest"
    f db a

checkTCP = NC.NagiosCommand { NC.command = "/usr/lib/nagios/plugins/check_tcp"
                  , NC.host = "jeremyhuffman.com"
                  , NC.port = Just 80 }
