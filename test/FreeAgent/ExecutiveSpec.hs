{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FreeAgent.ExecutiveSpec (main, spec) where

import           FreeAgent.Prelude
import           System.Process(system)
import           Test.Hspec

import           FreeAgent.Lenses
import           FreeAgent.Core
import           FreeAgent.Action
import           FreeAgent.Database
import           FreeAgent.Plugins.Nagios
import           FreeAgent.Executive as Exec

import           AppConfig(appConfig)

import           Control.Concurrent.Lifted
import           Control.Distributed.Process.Lifted

import           Control.Distributed.Process.Node
import           Network.Transport.TCP

import           Data.Dynamic

import           Control.Monad.Reader
import           Control.Exception

import qualified Data.Binary as Binary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "FreeAgent.Executive" $ do

        it "can execute a registered action" $ do
            testAgent $ \result -> do
                catchAny $ do
                    void Exec.init
                    Exec.execute $ RegisterAction $ Action checkTCP
                    threadDelay 10000
                    Exec.execute $ ExecuteRegistered $ key checkTCP
                    threadDelay 10000
                    -- confirm results were written
                    items <- fromAgentDB $ withKeySpace "agent:actions:localhost:17500" $ do
                        scan "" queryItems
                    result $ length items
                $ \exception ->
                    result $ throw exception
            `shouldReturn` 1

        it "can execute a supplied action" $ do
            testAgent $ \result -> do
                catchAny $ do
                    void Exec.init
                    Exec.execute $ ExecuteAction $ Action checkTCP
                    threadDelay 10000
                    -- confirm results were written
                    items <- fromAgentDB $ withKeySpace "agent:actions:localhost:17500" $ do
                        scan "" queryItems
                    result $ length items
                $ \exception ->
                    result $ throw exception
            `shouldReturn` 1


-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testAgent :: ((a -> Agent ()) -> Agent ()) -> IO a
testAgent ma = do
    setup
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
