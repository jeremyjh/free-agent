{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.StoreSpec (main, spec) where

import           BasicPrelude
import qualified Data.ByteString                  as BS
import           Test.Hspec
import           System.Process(system)
import           Dash.Proto
import           Dash.Store
import           Control.Monad.Trans.Resource      (release, ResIO, runResourceT, liftResourceT)
import qualified Dash.Plugins.Nagios.Proto.Command as NC
import           Dash.Runner
import           Dash.Plugins                      ()
import           Dash.Action

main :: IO ()
main = hspec spec

setup :: IO ()
setup = system ("rm -rf " ++ testDB) >> return ()

spec :: Spec
spec = do
    describe "Dash.Store" $ do
        it "setup" $ setup >>= shouldReturn (return())
        describe "has a simple API that" $ do
            it "writes Commands to the DB" $
                withDBT (stash checkTCP) >>= shouldReturn (return())
            it "reads Commands from the DB" $
                withDBT (fetch "jeremyhuffman.com") >>= shouldBe (Right checkTCP)
            it "writes wrapped Commands to the DB" $
                withDBT (stashWrapped checkTCP) >>= shouldReturn (return())
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
                withDBT (put "somekey" "somevalue") >>= shouldReturn (return ())
            it "will fail to deserialize if data is not a protobuf" $ do
                (Left (ParseFail msg)) <- withDBT (fetchProtoNC "somekey")
                take 25 msg `shouldBe` "Failed at 1 : Text.Protoc"
        describe "has a reader context API that" $ do
            it "is awesome" $ do
                (Just simple, Right proto)
                    <- runLevelDB testDB "awesome" $ do
                        put "thekey" "thevalue"
                        withKeySpace "otherspace" $ do
                            put "thekey" "othervalue"
                        simple <- get "thekey"
                        stashWrapped checkTCP
                        proto <- fetch "jeremyhuffman.com"
                        return (simple, join $ map unWrap proto)
                simple `shouldBe` "thevalue"
                proto `shouldBe` checkTCP
            it "can scan partial matches" $ do
                results <- runLevelDB testDB "scan" $ do
                    put "employee:1" "Jill"
                    put "employee:2" "Jack"
                    put "cheeseburgers:1" "do not want"
                    first <-  scan "employee:" queryItems
                    second <- scan "employee:" $
                                   queryList {scanMap = (\(k, v) -> v ++ " Smith")}
                    third  <-  scan "employee:"
                                   queryItems { scanFilter = (\(_, v) -> v > "Jack") }
                    fourth <- scan "employee:" $
                                   queryBegins { scanInit = 0
                                              , scanMap = (\(_, v) -> BS.head v)
                                              , scanReduce = (+)
                                              }
                    return (first, second, third, fourth)
                results `shouldBe` ( [("employee:1", "Jill"), ("employee:2", "Jack")]
                                   , [ "Jill Smith", "Jack Smith"]
                                   , [("employee:1", "Jill")]
                                   , 148)

testDB = "/tmp/leveltest"

withDBT :: LevelDB a -> IO a
withDBT = runLevelDB testDB "Dash.StoreSpec"

fetchProtoNC :: Key -> LevelDB (Either ProtoFail NC.Command)
fetchProtoNC = fetch

checkTCP = NC.Command { NC.command = "/usr/lib/nagios/plugins/check_tcp"
                  , NC.host = "jeremyhuffman.com"
                  , NC.port = Just 80 }
