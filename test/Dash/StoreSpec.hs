{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.StoreSpec (main, spec) where

import           BasicPrelude
import qualified Data.ByteString                  as BS
import           Test.Hspec
import           System.Process(system)
import           Dash.Store
import           Dash.Types
import           Control.Monad.Trans.Resource      (release, ResIO, runResourceT, liftResourceT)
import           Dash.Plugins.Nagios
import           Dash.Plugins.Common
import           Dash.Runner
import           Dash.Plugins
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
                withDBT (fetch "localhost") >>= shouldBe (Right checkTCP)
            it "writes wrapped Commands to the DB" $
                withDBT (stashWrapped checkTCP) >>= shouldReturn (return())
            it "reads Commands from the DB as Action" $ do
                action <- withDBT (fetchAction "localhost")
                action `shouldBe` (Right $ Action checkTCP)
            it "can read an Action from the DB and execute it" $ do
                (Right action) <- withDBT (fetchAction "localhost")
                exec action >>= shouldBe (Complete $ Just "Awesome")
            it "will fail to read if key is wrong" $ do
                (Left (NotFound _)) <- withDBT (fetchProto"notgonnamatch")
                True `shouldBe` True -- NOT exception
            it "can write an arbitrary bytestring" $
                withDBT (put "somekey" "somevalue") >>= shouldReturn (return ())
            it "will fail to deserialize if data is not a protobuf" $ do
                (Left (ParseFail msg)) <- withDBT (fetchProto"somekey")
                take 25 msg `shouldBe` "Failed reading: safecopy:"
        describe "has a reader context API that" $ do
            it "is awesome" $ do
                (Just simple, Right proto)
                    <- runCreateLevelDB testDB "awesome" $ do
                        put "thekey" "thevalue"
                        withKeySpace "otherspace" $ do
                            put "thekey" "othervalue"
                        simple <- get "thekey"
                        stashWrapped checkTCP
                        proto <- fetch "localhost"
                        return (simple, join $ map unWrap proto)
                simple `shouldBe` "thevalue"
                proto `shouldBe` checkTCP
            it "can scan and transform" $ do
                runCreateLevelDB testDB "scan" $ do
                    put "employee:1" "Jill"
                    put "employee:2" "Jack"
                    put "cheeseburgers:1" "do not want"
                    r1 <- scan "employee" queryItems
                    r2 <- scan "employee:" $
                                   queryList {scanMap = \ (k, v) -> v <> " Smith"}
                    r3 <- scan "employee:"
                                   queryItems { scanFilter = \ (_, v) -> v > "Jack" }
                    r4 <- scan "employee:" $
                                queryBegins   { scanInit = 0
                                              , scanMap = \ (_, v) -> BS.head v
                                              , scanFold = (+) }
                    return (r1, r2, r3, r4)
                `shouldReturn` ( [("employee:1", "Jill"), ("employee:2", "Jack")]
                               , [ "Jill Smith", "Jack Smith"]
                               , [("employee:1", "Jill")]
                               , 148)
        describe "supports batch operations such that" $ do
            it "can stash in a batch" $ do
                runCreateLevelDB testDB "stashbatch" $ do
                    runBatch $ do
                        stashB checkTCP
                        stashB checkTCP{host="awesome.com"}
                    fetch $ key checkTCP
                `shouldReturn` (Right checkTCP)
            it "can fetch a set" $ do
                runCreateLevelDB testDB "stashbatch" $ do
                    xs <- scanFetch ""
                    return xs
                `shouldReturn` ([Right checkTCP{host="awesome.com"}, Right checkTCP])

testDB = "/tmp/leveltest"

withDBT :: LevelDB a -> IO a
withDBT = runCreateLevelDB testDB "Dash.StoreSpec"

fetchProto:: Key -> LevelDB (Either FetchFail Command)
fetchProto= fetch

checkTCP = Command { command = "/usr/lib/nagios/plugins/check_tcp"
                  , host = "localhost"
                  , port = Just 17500 }
