{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgent.PeerSpec (main, spec) where

import           AgentPrelude
import           FreeAgent.Core
import           FreeAgent.Process
import           FreeAgent.Lenses
import           AppConfig(appConfig)

import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.Lifted

import           Control.Exception
import           Test.Hspec
import           FreeAgent

main :: IO ()
main = hspec spec


spec :: Spec
spec =
    describe "FreeAgent.Peer" $ do
        it "is started by Core supervisor" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Just pid <- whereis $ peerServer^.name
                    result True
                $ \exception ->
                    result $ throw exception
            `shouldReturn` True

-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testAgent :: ((a -> Agent ()) -> Agent ()) -> IO a
testAgent ma = do
    result <- newEmptyMVar
    runAgent appConfig (ma (putMVar result))
    threadDelay 2000 -- so we dont get open port errors
    takeMVar result
