{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables                                    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module FreeAgent.CoreSpec (main, spec) where


import           FreeAgent.AgentPrelude
import           FreeAgent.Core
import           Test.Hspec

import           FreeAgent.Fixtures
import           FreeAgent.TestHelper


import           Control.Concurrent.Lifted

import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Yaml                 as Yaml


main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "C.D.Process primitives in Agent monad" $
            it "can do basic Process messaging" $ testAgent (
                 do parent <- getSelfPid
                    child <- spawnLocal $ do
                        saysee <- texpect :: Agent ByteString
                        send parent ("I said: " ++ saysee)
                    send child ("foo" :: ByteString)
                    texpect :: Agent ByteString
                ) `shouldReturn` "I said: foo"

    describe "Binary (de)serialization of existential Action type" $ do
            it "can send/receive an Action and Result" $ testAgent (
                 do parent <- getSelfPid
                    child <- forkApp2 $ do
                            action <- texpect :: Agent Action
                            (Right nr) <- exec action
                            send parent nr
                    send child $ toAction checkTCP
                    nr <- texpect
                    let Just (NagiosResult _ ok) = extractResult nr
                    return ok
                ) `shouldReturn` OK

            it "can send the result of an Action" $ testAgent (
                 do (Right nr) <- exec $ toAction checkTCP
                    parent <- getSelfPid
                    child <- forkApp2 $ do
                        wr <- texpect :: Agent Result
                        let Just (NagiosResult _noex OK) = extractResult wr
                        send parent ("Got OK" :: Text)
                    send child nr
                    texpect :: Agent Text
                ) `shouldReturn` "Got OK"

    describe "Json (de)serialization" $ do
            it "works about the same as Binary" $ testAgent (
                 do let yaction = Yaml.encode $ toAction checkTCP
                    let Just action' :: Maybe Action =  Yaml.decode yaction

                    Right creturn <- exec action'
                    let yreturn = Yaml.encode creturn
                    Just (_::Result) <- return $ Yaml.decode yreturn

                    return (resultResultOf creturn)
                ) `shouldReturn` toAction checkTCP

            it "works for encoded Results" $ testAgent (
                 do (Right nr) <- exec $ toAction checkTCP
                    parent <- getSelfPid
                    child <- forkApp2 $ do
                        wr <- texpect :: Agent Result
                        decoded <- decodeResult wr
                        send parent (Yaml.toJSON decoded)
                    send child nr
                    Yaml.Object obj <- texpect :: Agent Yaml.Value
                    let Just (Yaml.Object bytes) = HashMap.lookup "resultWrapped" obj
                    return $ isJust $ HashMap.lookup "value" bytes
                ) `shouldReturn` True

forkApp2 ma =
 do childPid <-  newEmptyMVar
    void $ fork $ liftIO $
     runAgentPool 500 $
        getSelfPid >>= putMVar childPid >> ma
    takeMVar childPid
