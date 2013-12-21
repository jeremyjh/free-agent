{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BangPatterns #-}
module FreeAgent.ActionSpec (main, spec) where

import           FreeAgent.Prelude
import qualified Data.ByteString                  as BS
import           Data.Map
import           Control.Monad.Reader


import           System.Process(system)

import           Test.Hspec

import           FreeAgent.Lenses
import           FreeAgent.Core
import           FreeAgent.Action
import           FreeAgent.Database
import           FreeAgent.Plugins.Nagios as Nagios
import           AppConfig(appConfig)
import           Control.Concurrent (threadDelay)

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
            it "writes Commands to the DB" $ do
                withConfig $  fromAgentDB $ stash checkTCP
                `shouldReturn` ()
            it "reads Commands from the DB" $
                withConfig (fromAgentDB (fetch "localhost"))
                `shouldReturn` (Right checkTCP)
            describe "has special storage of Actions" $ do
                it "writes Actions to the DB as wrapped for fetchAction" $ do
                    let act = Action checkTCP
                    withConfig (withAgentDB (stashAction act) )
                    `shouldReturn` ()
                it "reads wrapped Actions from the DB" $ do
                    -- would fail if previous spec did not wrap
                    action <- withConfig $ fromAgentDB $ fetchAction "localhost"
                    action `shouldBe` (Right $ Action checkTCP)
                it "will fail to read if key is wrong" $ do
                    (Left (NotFound _)) <- withConfig $ fromAgentDB (fetchProto "notgonnamatch")
                    True `shouldBe` True -- NOT exception
                it "can write an arbitrary bytestring" $
                    withConfig (withAgentDB . withKeySpace "agent:actions" $
                        (put "somekey" "somevalue"))
                    `shouldReturn` ()
                it "will fail to deserialize if data is not a valid Serialized" $ do
                    (Left (ParseFail msg)) <- withConfig $ fromAgentDB $ fetchAction "somekey"
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
                        ctxt <- askConfig
                        fromAgentDB $ do
                            stashAction $ Action checkTCP
                            stashAction $ Action checkTCP {_commandHost = "check2"}
                            stashAction $ Action checkTCP {_commandHost = "check3"}
                            scanActions ctxt "check"
                    `shouldReturn` [ (Right $ Action checkTCP{_commandHost="check2"} )
                                   , (Right $ Action checkTCP{_commandHost="check3"}) ]
        it "can be matched" $ do
            let matcher = matches (\c -> (_commandHost c) == "localhost")
            matcher (Action checkTCP) `shouldBe` True



testDB = appConfig^.agentConfig.dbPath

withConfig :: ReaderT AgentContext IO a -> IO a
withConfig ma = do
    registerPluginMaps $ (appConfig^.actionMap, appConfig^.resultMap)
    (_, dbChan) <- initAgentDB appConfig
    let ctxt' = appConfig & agentDBChan .~ dbChan
    v <- runReaderT ma ctxt'
    closeAgentDB dbChan
    threadDelay 10000
    return v



fetchProto:: Key -> LevelDB (Either FetchFail Command)
fetchProto= fetch

checkTCP = Command  "localhost" (Just 17500) "check_tcp"

setup :: IO ()
setup = void $ system ("rm -rf " ++ appConfig^.agentConfig.dbPath)
