{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.CoreSpec (main, spec) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core
import           Test.Hspec

import           FreeAgent.Core.Lenses
import           FreeAgent.Plugins.Nagios

import           FreeAgent.TestHelper
import           FreeAgent.Fixtures

import qualified Data.Yaml  as Yaml
import           FreeAgent.Process


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "C.D.Process primitives in Agent monad" $ do
            it "can run a process" $ do
                testAgent $  do
                    pid <- getSelfPid
                    return $ take 3 $ tshow pid
                `shouldReturn` "pid"

            it "can do basic Process messaging" $ do
                testAgent $  do
                    parent <- getSelfPid
                    child <- spawnLocal $ do
                        saysee <- texpect :: Agent ByteString
                        send parent ("I said: " ++ saysee)
                    send child ("foo" :: ByteString)
                    said <- texpect :: Agent ByteString
                    return said
                `shouldReturn` "I said: foo"

    describe "Binary (de)serialization of existential Action type" $ do
            it "can send/receive an Action" $ do
                testAgent $  do
                    parent <- getSelfPid
                    child <- spawnLocal $ do
                        action <- texpect :: Agent Action
                        (Right nr) <- exec action
                        let Just (NagiosResult _ OK) = extract nr
                        send parent ("Got OK" :: Text)
                    send child $ Action checkTCP
                    confirm <- texpect :: Agent Text
                    return confirm
                `shouldReturn` "Got OK"

            it "can send the return of an Action" $ do
                testAgent $ do
                    (Right nr) <- exec $ Action checkTCP
                    parent <- getSelfPid
                    child <- spawnLocal $ do
                        wr <- texpect :: Agent Result
                        let Just (NagiosResult _ OK) = extract wr
                        send parent ("Got OK" :: Text)
                    send child nr
                    confirm <- texpect :: Agent Text
                    return confirm
                `shouldReturn` "Got OK"

    describe "Json (de)serialization of existential Action type" $ do
            it "works about the same as Binary" $ do
                testAgent $  do
                    let yaction = Yaml.encode $ Action checkTCP
                    let Just action' = Yaml.decode yaction

                    Right creturn <- exec $ Action checkTCP
                    let yreturn = Yaml.encode creturn
                    let Just (_::Result) = Yaml.decode yreturn

                    return action'
                `shouldReturn` Action checkTCP

testAgent ma = testRunAgent setup 1000 appConfig appPlugins ma
