{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.CoreSpec (main, spec) where

import           AgentPrelude
import           Test.Hspec

import           FreeAgent.Lenses
import           FreeAgent.Plugins.Nagios

import           FreeAgent.TestHelper

import qualified Data.Yaml  as Yaml
import           FreeAgent.Process
import           Control.Exception


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "C.D.Process primitives in Agent monad" $ do
            it "can run a process" $ do
                testAgent $ \ result -> do
                    pid <- getSelfPid
                    result $ take 3 $ tshow pid
                `shouldReturn` "pid"

            it "can do basic Process messaging" $ do
                testAgent $ \ result -> do
                    parent <- getSelfPid
                    child <-  spawnLocal $ do
                        saysee <- texpect :: Agent ByteString
                        send parent ("I said: " ++ saysee)
                    send child ("foo" :: ByteString)
                    said <- texpect :: Agent ByteString
                    result said
                `shouldReturn` "I said: foo"

    describe "Binary (de)serialization of existential Action type" $ do
            it "can send/receive an Action" $ do
                testAgent $ \ result -> do
                    catchAny $ do
                        parent <- getSelfPid
                        child <-  spawnLocal $ do
                            action <- texpect :: Agent Action
                            (Right nr) <- exec action
                            let Just (NagiosResult _ OK) = extract nr
                            send parent ("Got OK" :: Text)
                        send child $ Action checkTCP
                        confirm <- texpect :: Agent Text
                        result confirm
                    $ \exception ->
                        result $ throw exception
                `shouldReturn` "Got OK"

            it "can send the result of an Action" $ do
                testAgent $ \result -> do
                    catchAny $ do
                        (Right nr) <- exec $ Action checkTCP
                        parent <- getSelfPid
                        child <- spawnLocal $ do
                            wr <- texpect :: Agent Result
                            let Just (NagiosResult _ OK)= extract wr
                            send parent ("Got OK" :: Text)
                        send child nr
                        confirm <- texpect :: Agent Text
                        result confirm
                    $ \exception ->
                        result $ throw exception
                `shouldReturn` "Got OK"


    describe "Json (de)serialization of existential Action type" $ do
            it "works about the same as Binary" $ do
                testAgent $ \ result -> do
                    catchAny $ do
                        let yaction = Yaml.encode $ Action checkTCP
                        let Just action' = Yaml.decode yaction

                        Right cresult <- exec $ Action checkTCP
                        let yresult = Yaml.encode cresult
                        let Just (_::Result) = Yaml.decode yresult

                        result action'
                    $ \exception ->
                        result $ throw exception
                `shouldReturn` Action checkTCP

testAgent ma = testRunAgent setup appConfig appPlugins ma

-- for testing - useful to throw an exception if we "never" get the value we're expecting
texpect :: (MonadProcess m, Monad m) => forall a. NFSerializable a => m a
texpect = do
    gotit <- expectTimeout 50000 -- 100ms may as well be never
    case gotit of
        Nothing -> error "Timed out in test expect"
        Just v -> return v
