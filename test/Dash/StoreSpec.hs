{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.StoreSpec (main, spec) where

import           BasicPrelude
import qualified Data.ByteString                  as BS
import           Data.Map
import           Control.Monad.Reader
import           System.Process(system)

import           Test.Hspec

import           Dash.Store
import           Dash.Types
import           Dash.Plugins.Nagios as Nagios
import           Dash.Plugins

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
                action <- withDBT (runConfig $ fetchAction "localhost")
                action `shouldBe` (Right $ Action checkTCP)
            it "can read an Action from the DB and execute it" $ do
                (Right action) <- withDBT (runConfig $ fetchAction "localhost")
                exec action >>= shouldBe (Complete $ Just "Awesome")
            it "will fail to read if key is wrong" $ do
                (Left (NotFound _)) <- withDBT (fetchProto"notgonnamatch")
                True `shouldBe` True -- NOT exception
            it "can write an arbitrary bytestring" $
                withDBT (put "somekey" "somevalue") >>= shouldReturn (return ())
            it "will fail to deserialize if data is not a protobuf" $ do
                (Left (ParseFail msg)) <- withDBT (fetchProto"somekey")
                take 25 msg `shouldBe` "Failed reading: safecopy:"
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

myAppConfig = AgentConfig {configPlugins = fromList Nagios.registerUnWrappers}

runConfig ma = runReaderT ma myAppConfig

withDBT :: LevelDB a -> IO a
withDBT = runCreateLevelDB testDB "Dash.StoreSpec"

fetchProto:: Key -> LevelDB (Either FetchFail Command)
fetchProto= fetch

checkTCP = Command { command = "/usr/lib/nagios/plugins/check_tcp"
                  , host = "localhost"
                  , port = Just 17500 }
