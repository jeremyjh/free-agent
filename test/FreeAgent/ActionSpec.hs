{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgent.ActionSpec (main, spec) where

import           FreeAgent.Prelude
import qualified Data.ByteString                  as BS
import           Data.Map
import           Control.Monad.Reader


import           System.Process(system)

import           Test.Hspec

import           Database.LevelDB.Higher

import           FreeAgent.Lenses
import           FreeAgent.Core
import           FreeAgent.Action
import           FreeAgent.Plugins.Nagios as Nagios
import           AppConfig(appConfig)

import qualified Data.Binary as Binary

--debug
import           Debug.Trace
debug :: (Show a) => a -> a
debug a = traceShow a a
--debug

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "FreeAgent.Action" $ do
        it "setup" $ setup >>= shouldReturn (return())
        describe "uses basic storage functionality from higher-leveldb" $ do
            it "writes Commands to the DB" $
                withDBT (stash checkTCP) >>= shouldReturn (return())
            it "reads Commands from the DB" $
                withDBT (fetch "localhost") >>= shouldBe (Right checkTCP)
            describe "has special storage of Actions" $ do
                it "writes Actions to the DB as wrapped for fetchAction" $ do
                    let act = toAction checkTCP
                    withDBT (stashAction act) >>= shouldReturn (return())
                it "reads wrapped Actions from the DB" $ do
                    -- would fail if previous spec did not wrap
                    action <- withConfig $ fetchAction "localhost"
                    action `shouldBe` (Right $ toAction checkTCP)
                it "will fail to read if key is wrong" $ do
                    (Left (NotFound _)) <- withDBT (fetchProto "notgonnamatch")
                    True `shouldBe` True -- NOT exception
                it "can write an arbitrary bytestring" $
                    withDBT (put "somekey" "somevalue") >>= shouldReturn (return ())
                it "will fail to deserialize if data is not a valid Serialized" $ do
                    (Left (ParseFail msg)) <- withConfig $ fetchAction "somekey"
                    take 25 msg `shouldBe` "too few bytes\nFrom:\tdeman"
            describe "supports batch operations such that" $ do
                it "can stashAction in a batch" $ do
                    runCreateLevelDB testDB "stashActionbatch" $ do
                        runBatch $ do
                            stash checkTCP
                            stash checkTCP{_commandHost="awesome.com"}
                        fetch $ key checkTCP
                    `shouldReturn` (Right checkTCP)
                it "can fetch a set" $ do
                    runCreateLevelDB testDB "stashActionbatch" $ do
                        xs <- scanFetch ""
                        return xs
                    `shouldReturn` ([Right checkTCP{_commandHost="awesome.com"}, Right checkTCP])
                it "can scan a set of actions" $ do
                    withConfig $ do
                        stashAction $ toAction checkTCP
                        stashAction $ toAction checkTCP {_commandHost = "check2"}
                        stashAction $ toAction checkTCP {_commandHost = "check3"}
                        scanActions "check"
                    `shouldReturn` [ (Right $ toAction checkTCP{_commandHost="check2"} )
                                   , (Right $ toAction checkTCP{_commandHost="check3"}) ]


testDB = appConfig^.agentConfig.dbPath

withConfig ma = do
    registerPluginMaps $ (appConfig^.actionMap, appConfig^.resultMap)
    withDBT $ runReaderT ma appConfig

withDBT :: LevelDB a -> IO a
withDBT = runCreateLevelDB testDB "agent:actions"


fetchProto:: Key -> LevelDB (Either FetchFail Command)
fetchProto= fetch

checkTCP = Command  "localhost" (Just 17500) "check_tcp"

setup :: IO ()
setup = void $ system ("rm -rf " ++ appConfig^.agentConfig.dbPath)
