{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dash.CoreSpec (main, spec) where

import           Dash.Prelude
import           System.Process(system)
import           Test.Hspec

import           Dash.Lenses
import           Dash.Core
import           AppConfig(appConfig)

import           Control.Concurrent.Lifted
import           Control.Distributed.Process.Lifted

import          Control.Distributed.Process.Node
import          Network.Transport.TCP

import          Control.Monad.Reader
import          Database.LevelDB.Higher
import          Control.Exception

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Dash.Core" $ do
        it "setup" $ do setup >>= shouldReturn(return())

        describe "works with Agent" $ do
            it "can run a process" $ do
                testAgent $ \ result -> do
                    pid <- getSelfPid
                    result $ take 3 $ tshow pid
                `shouldReturn` "pid"
            it "can do basic Process messaging" $ do
                testAgent $ \ result -> do
                    parent <- getSelfPid
                    child <-  spawnAgent $ do
                        saysee <- expect :: Agent ByteString
                        send parent ("I said: " ++ saysee)
                    send child ("foo" :: ByteString)
                    said <- expect :: Agent ByteString
                    result said
                `shouldReturn` "I said: foo"


-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testAgent :: ((a -> Agent ()) -> Agent ()) -> IO a
testAgent ma = do
    result <- newEmptyMVar
    runAgent appConfig (ma (putMVar result))
    threadDelay 2000 -- so we dont get open port errors
    takeMVar result

setup :: IO ()
setup = void $ system ("rm -rf " ++ appConfig^.dbPath)
