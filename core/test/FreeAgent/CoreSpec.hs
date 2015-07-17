{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables                                    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module FreeAgent.CoreSpec (main, spec) where


import           FreeAgent.AgentPrelude
import           FreeAgent.Core
import           FreeAgent.Core.Lenses
import           FreeAgent.Server          (runAgentServers)
import           Test.Hspec

import qualified Data.Binary               as Binary

import           FreeAgent.Fixtures
import           FreeAgent.TestHelper


import           Control.Concurrent.Lifted

import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Yaml                 as Yaml


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "C.D.Process primitives in Agent monad" $ do
            it "can run a process" $ do
                testAgent $  do
                    pid <- getSelfPid
                    return $ take 3 $ show pid
                `shouldReturn` "pid"

            it "can do basic Process messaging" $ do
                testAgent $  do
                    parent <- getSelfPid
                    child <- spawnLocal $ do
                        saysee <- texpect :: Agent ByteString
                        send parent ("I said: " ++ saysee)
                    send child ("foo" :: ByteString)
                    texpect :: Agent ByteString
                `shouldReturn` "I said: foo"

    describe "Binary (de)serialization of existential Action type" $ do
            it "can send/receive an Action and Result" $ do
                testAgent $  do
                    parent <- getSelfPid
                    child <- forkApp2 $ do
                            action <- texpect :: Agent Action
                            (Right nr) <- exec action
                            send parent nr
                    send child $ toAction checkTCP
                    nr <- texpect
                    let Just (NagiosResult _ ok) = extractResult nr
                    return ok
                `shouldReturn` OK

            it "can send the result of an Action" $ do
                testAgent $ do
                    (Right nr) <- exec $ toAction checkTCP
                    parent <- getSelfPid
                    child <- forkApp2 $ do
                        wr <- texpect :: Agent Result
                        let Just (NagiosResult _ OK) = extractResult wr
                        send parent ("Got OK" :: Text)
                    send child nr
                    texpect :: Agent Text
                `shouldReturn` "Got OK"

    describe "Json (de)serialization" $ do
            it "works about the same as Binary" $ do
                testAgent $  do
                    let yaction = Yaml.encode $ toAction checkTCP
                    let Just action' = Yaml.decode yaction

                    Right creturn <- exec $ toAction checkTCP
                    let yreturn = Yaml.encode creturn
                    Just (r::Result) <- return $ Yaml.decode yreturn

                    return action'
                `shouldReturn` toAction checkTCP

            it "works for encoded Results" $ do
                testAgent $ do
                    (Right nr) <- exec $ toAction checkTCP
                    parent <- getSelfPid
                    child <- forkApp2 $ do
                        wr <- texpect :: Agent Result
                        send parent (Yaml.toJSON wr)
                    send child nr
                    Yaml.Object obj <- texpect :: Agent Yaml.Value
                    let Just (Yaml.Object bytes) = HashMap.lookup "resultWrapped" obj
                    return $ isJust $ HashMap.lookup "bytes" bytes
                `shouldReturn` True

testAgent ma = testRunAgent setup 1000 appConfig appPlugins ma

forkApp2 ma = forkAgentNode appConfig2 appPlugins ma

forkAgentNode config plugs ma =
 do childPid <-  newEmptyMVar
    void $ fork $ liftIO $
      runAgentServers config plugs $
        getSelfPid >>= putMVar childPid >> ma
    takeMVar childPid

appConfig2 :: AgentConfig
appConfig2 = appConfig
      & nodePort .~ "9092"
      & peerNodeSeeds .~ ["127.0.0.1:3546"]
      {-& minLogLevel .~ LevelInfo-}
