{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FreeAgent.ExecutiveSpec (main, spec) where

import           FreeAgent.Prelude
import           System.Process(system)
import           Test.Hspec

import           FreeAgent.Lenses
import           FreeAgent.Core
import           FreeAgent.Action
import           FreeAgent.Plugins.Nagios
import           FreeAgent.Executive as Exec

import           AppConfig(appConfig)

import           Control.Concurrent.Lifted
import           Control.Distributed.Process.Lifted

import           Control.Distributed.Process.Node
import           Network.Transport.TCP

import           Data.Dynamic

import           Control.Monad.Reader
import           Database.LevelDB.Higher as DB
import           Control.Exception

import qualified Data.Binary as Binary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "FreeAgent.Executive" $ do
        it "setup" $ do setup >>= shouldReturn(return())

        it "can execute an action" $ do
            testAgent $ \result -> do
                catchAny $ do
                    epid <- Exec.init
                    Exec.execute $ ExecuteAction $ toAction checkTCP
                    threadDelay 10000
                    -- confirm results were written
                    items <- withKeySpace "agent:actions:localhost:17500" $ do
                        DB.scan "" queryItems
                    result $ tshow $ length items
                $ \exception ->
                    result $ "Exception: " ++ tshow exception
            `shouldReturn` "1"


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
