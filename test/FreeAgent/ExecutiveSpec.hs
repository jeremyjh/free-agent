{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module FreeAgent.ExecutiveSpec (main, spec) where

import           Test.Hspec

import           AgentPrelude
import           FreeAgent.Lenses
import           FreeAgent.Core
import           FreeAgent.Action
import           FreeAgent.Database
import qualified FreeAgent.Database.KeySpace as KS
import           FreeAgent.Plugins.Nagios as Nagios
import           FreeAgent.Server.Executive as Exec
import           FreeAgent.Server (runAgentServers)

import           System.Process(system)
import qualified Data.Set as Set

import           Control.Concurrent.Lifted
import           FreeAgent.Process as Process
import           Control.Distributed.Process.Node
import Control.Distributed.Process.Closure (mkClosure)
import           Network.Transport.TCP
import           Data.Dynamic

import           Control.Exception

import qualified Data.Binary as Binary
import qualified Data.Serialize as Cereal

matchRemoteHostName :: ProcessId -> Listener
matchRemoteHostName pid = matchAction (\c -> _checktcpHost c == "localhost") pid
remotable ['matchRemoteHostName]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "FreeAgent.Executive" $ do

        it "is started by Core supervisor" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Just pid <- whereis $ execServer^.name
                    result True
                $ \exception ->
                    result $ throw exception
            `shouldReturn` True

        it "can execute a registered action" $ do
            testAgent $ \result -> do
                catchAny $ do
                    registerAction $ Action checkTCP
                    (Right nr) <- executeRegistered $ key checkTCP
                    threadDelay 10000
                    -- confirm results were written
                    items <- agentDb $ withKeySpace "agent:actions:localhost:53" $ do
                        scan "" queryItems
                    result $ length items
                $ \exception ->
                    result $ throw exception
            `shouldReturn` 1

        it "can execute a supplied action" $ do
            testAgent $ \result -> do
                catchAny $ do
                    (Right nr) <- executeAction $ Action checkTCP
                    let Just (NagiosResult _ OK) = extract nr
                    -- confirm results were written
                    items <- agentDb $ withKeySpace "agent:actions:localhost:53" $ do
                        scan "" queryItems
                    result $ length items
                $ \exception ->
                    result $ throw exception
            `shouldReturn` 1

        it "will invoke any configured listeners" $ do
            testAgent $ \result -> do
                catchAny $ do
                    -- generate some results to hear about
                    forM_ [0..2] $ \_ ->
                        executeAction $ Action checkTCP
                    -- make sure he's been listening
                    pid <- getSelfPid
                    nsend "ExecSpecActionListener" ("ask-result-count" :: String, pid)
                    actionCount <- expect :: Agent Int
                    nsend "ExecSpecResultListener" ("ask-result-count" :: String, pid)
                    resultCount <- expect :: Agent Int
                    result (actionCount, resultCount)
                $ \exception ->
                    result $ throw exception
            `shouldReturn` (3,3)

        it "can subscribe listeners at runtime" $ do
            testAgent $ \result -> do
                catchAny $ do
                    pid <- getSelfPid
                    let matcher = $(mkClosure 'matchRemoteHostName) pid
                    addRemoteListener execServer matcher
                    threadDelay 10000
                    executeAction $ Action checkTCP
                    nr <- expect :: Agent Result
                    let Just (NagiosResult _ status) = extract nr
                    result status
                $ \exception ->
                    result $ throw exception
            `shouldReturn` OK

        it "can deliver and remove a basic package" $ do
            testAgent $ \result -> do
                catchAny $ do
                    package <- set actions [Action checkTCP] <$> defaultPackage
                    results <- deliverPackage package

                    -- confirm package was added
                    (Just _) <- agentDb $ withKeySpace KS.packages $
                        get $ key package

                    -- confirm results were written
                    resultsAdded <- agentDb $
                        withKeySpace "agent:actions:localhost:53" $ do
                            length <$> scan "" queryItems

                    -- confirm history was added
                    historyAdded <- agentDb $
                        withKeySpace ("agent:packages:" ++ key package) $ do
                            length <$> scan "" queryItems

                    -- now test remove
                    removePackage $ package^.uuid

                    Nothing <- agentDb $ withKeySpace KS.packages $ do
                        get $ key package

                    result $ (length results, resultsAdded, historyAdded)
                $ \exception ->
                    result $ throw exception
            `shouldReturn` (1,1,1)

        it "it won't deliver a package for an unknown context or zone" $ do
            testAgent $ \result -> do
                catchAny $ do
                    let unknownContext = Set.fromList [Context "unknown"]
                    package <- set actions [Action checkTCP] <$> defaultPackage
                                   & fmap (set contexts unknownContext)

                    results <- deliverPackage package

                    -- confirm package was not added
                    Nothing <- agentDb $ withKeySpace KS.packages $
                        get $ key package

                    let unknownZone = Set.fromList [Zone "unknown"]
                    package' <- set actions [Action checkTCP] <$> defaultPackage
                                    & fmap (set zones unknownZone)

                    deliverPackage package'

                    -- confirm package was not added
                    Nothing <- agentDb $ withKeySpace KS.packages $
                        get $ key package'

                    result $ True
                $ \exception ->
                    result $ throw exception
            `shouldReturn` True

-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testAgent :: ((a -> Agent ()) -> Agent ()) -> IO a
testAgent ma = do
    setup
    result <- newEmptyMVar
    runAgentServers appConfig (ma (putMVar result))
    threadDelay 2000 -- so we dont get open port errors
    takeMVar result


-- for testing - useful to throw an exception if we "never" get the value we're expecting
texpect :: (MonadProcess m, Monad m) => forall a. NFSerializable a => m a
texpect = do
    gotit <- expectTimeout 10000 -- 100ms may as well be never
    case gotit of
        Nothing -> error "Timed out in test expect"
        Just v -> return v

setup :: IO ()
setup = void $ system ("rm -rf " ++ appConfig^.agentConfig.dbPath)


checkTCP = CheckTCP  "localhost" 53



testActionListener :: Agent [Listener]
testActionListener = do
    apid <- liftProcess $ spawnLocal $ loop (0::Int)
    Process.register "ExecSpecActionListener" apid
    return [matchAction (\c -> _checktcpHost c == "localhost") apid]
  where
    loop count = do
        receiveWait
            [ match $ \ (result :: Result) -> do
                  loop $ count + 1
            , match $ \ ("ask-result-count" :: String, pid) ->
                  send pid count
            ]

testResultListener :: Agent [Listener]
testResultListener = do
    rpid <- liftProcess $ spawnLocal $ loop (0::Int)
    Process.register "ExecSpecResultListener" rpid
    return [matchResult (const True)
                        (\ (NagiosResult _ r) -> r == OK)
                        rpid
           ]
  where
    loop count = do
        receiveWait
            [ match $ \ (result :: Result) ->
                  loop $ count + 1
            , match $ \ ("ask-result-count" :: String, pid) ->
                  send pid count
            ]



testDef :: NagiosConfig -> PluginDef
testDef conf = definePlugin "ExecSpec"
                            conf
                            ((++) <$> testActionListener <*> testResultListener)
                            (return ())


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
      & appendRemoteTable __remoteTable
      {-& agentConfig.minLogLevel .~ LevelDebug-}
