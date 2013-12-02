{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FreeAgent.CoreSpec (main, spec) where

import           FreeAgent.Prelude
import           System.Process(system)
import           Test.Hspec

import           FreeAgent.Lenses
import           FreeAgent.Core
import           FreeAgent.Action
import           FreeAgent.Plugins.Nagios

import           AppConfig(appConfig)

import           Control.Concurrent.Lifted
import           Control.Distributed.Process.Lifted

import           Control.Distributed.Process.Node
import           Network.Transport.TCP

import           Data.Dynamic

import           Control.Monad.Reader
import           Database.LevelDB.Higher
import           Control.Exception

import qualified Data.Binary as Binary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "FreeAgent.Core" $ do
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
                        saysee <- texpect :: Agent ByteString
                        send parent ("I said: " ++ saysee)
                    send child ("foo" :: ByteString)
                    said <- texpect :: Agent ByteString
                    result said
                `shouldReturn` "I said: foo"

            it "can read a wrapped Action from the DB and execute it" $ do
                testAgent $ \result -> do
                    catchAny $ do
                        stashAction $ toAction checkTCP
                        (Right action) <- fetchAction "localhost:17500"
                        (Right nr) <- exec action
                        let Just (OK rs) = extract nr
                        result $ take 6 rs
                    $ \exception ->
                        result $ "Exception: " ++ tshow exception
                `shouldReturn` "TCP OK"

            it "can send an action result to a listener as a concrete type" $ do
                testAgent $ \result -> do
                    catchAny $ do
                        -- could just get a concrete from exec
                        (Right (OK _)) <- exec checkTCP
                         -- but ... need to test existential deliver for this spec
                        (Right nr) <- exec $ toAction checkTCP
                        parent <- getSelfPid
                        child <-  spawnAgent $ do
                            (OK _) <- texpect
                            send parent ("Got OK" :: Text)
                        deliver nr child
                        confirm <- texpect
                        result confirm
                    $ \exception ->
                        result $ "Exception: " ++ tshow exception
                `shouldReturn` "Got OK"

            it "can send a result action to a listener as an existential type" $ do
                testAgent $ \result -> do
                    catchAny $ do
                        (Right nr) <- exec $ toAction checkTCP
                        parent <- getSelfPid
                        child <-  spawnAgent $ do
                            wr <- texpect :: Agent ActionResult
                            let Just (OK _) = extract wr
                            send parent ("Got OK" :: Text)
                        send child nr
                        confirm <- texpect
                        result confirm
                    $ \exception ->
                        result $ "Exception: " ++ tshow exception
                `shouldReturn` "Got OK"

            it "can send/receive an action" $ do
                testAgent $ \ result -> do
                    catchAny $ do
                        parent <- getSelfPid
                        child <-  spawnAgent $ do
                            action <- texpect :: Agent Action
                            (Right nr) <- exec action
                            let Just (OK _) = extract nr
                            send parent ("Got OK" :: Text)
                        send child $ toAction checkTCP
                        confirm <- texpect
                        result confirm
                    $ \exception ->
                        result $ "Exception: " ++ tshow exception
                `shouldReturn` "Got OK"

-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testAgent :: ((a -> Agent ()) -> Agent ()) -> IO a
testAgent ma = do
    result <- newEmptyMVar
    runAgent appConfig (ma (putMVar result))
    threadDelay 2000 -- so we dont get open port errors
    takeMVar result

-- for testing - useful to throw an exception if we "never" get the value we're expecting
texpect :: (MonadProcess m, Monad m) => forall a. Serializable a => m a
texpect = do
    gotit <- expectTimeout 10000 -- 100ms may as well be never
    case gotit of
        Nothing -> error "Timed out in test expect"
        Just v -> return v

setup :: IO ()
setup = void $ system ("rm -rf " ++ appConfig^.agentConfig.dbPath)

checkTCP = CheckTCP  "localhost" 17500
