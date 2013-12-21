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
import           FreeAgent.Plugins.Nagios as Nagios
import           FreeAgent.Executive as Exec

import           Control.Concurrent.Lifted
import           Control.Distributed.Process.Lifted as Process

import           Control.Distributed.Process.Node
import           Network.Transport.TCP

import           Data.Dynamic

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

        it "will invoke any configured listeners" $ do
            testAgent $ \result -> do
                catchAny $ do
                    void Exec.init
                    -- generate some results to hear about
                    forM_ [0..2] $ \_ ->
                        Exec.execute $ ExecuteAction $ Action checkTCP
                    threadDelay 10000

                    -- make sure he's been listening
                    Just lpid <- whereis "ExecSpecListener"
                    pid <- getSelfPid
                    send lpid ("ask-result-count" :: ByteString, pid)
                    resCount <- expect :: Agent Int
                    result resCount
                $ \exception ->
                    result $ throw exception
            `shouldReturn` 3


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

-- use a local config here because we are wiring up our own test listener
appConfig :: AgentContext
appConfig = (
    registerPlugins $ do
        addPlugin $ Nagios.pluginDef def {
            -- override default plugin-specific config
            _nagiosPluginsPath = "/usr/lib/nagios/plugins"
        }
        addPlugin $ testDef def
        -- add more plugins here!
    ) & agentConfig.dbPath .~ "/tmp/leveltest" -- override Agent config values here!

testListener :: Agent [ActionListener]
testListener = do
    let listenLocal resCount = do
        receiveWait [
            match (\ (result :: ActionResult) -> do
                listenLocal $ resCount + 1
            ),
            match (\ ("ask-result-count" :: ByteString, pid) ->
                send pid resCount
            )
            ]
    lpid <- liftProcess $ spawnLocal $ listenLocal (0::Int)
    Process.register "ExecSpecListener" lpid
    return [(matches (\c -> _checktcpHost c == "localhost"), lpid)]

testDef :: NagiosConfig -> PluginDef
testDef conf = definePlugin "ExecSpec" conf testListener (return ())
