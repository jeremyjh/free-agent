{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults#-}


module FreeAgent.ExecutiveSpec (main, spec) where

import           Test.Hspec

import           AgentPrelude
import           FreeAgent.Lenses
import           FreeAgent.Core
import           FreeAgent.Server.Peer (CallFail(..))
import           FreeAgent.Server.Executive.History
import           FreeAgent.Plugins.Nagios as Nagios
import           FreeAgent.Server.Executive as Exec

import           FreeAgent.TestHelper hiding (appConfig, appPlugins)
import qualified FreeAgent.TestHelper as Helper

import           Control.Concurrent.Lifted
import           FreeAgent.Process as Process

import Control.Exception (throw)

matchRemoteHostName :: (NodeId, String) -> Listener
matchRemoteHostName (nodeid, name') = matchAction (\c -> _checktcpHost c == "localhost") nodeid name'
remotable ['matchRemoteHostName]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "FreeAgent.Executive" $ do

        it "is started by Core supervisor" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Just _ <- resolve execServer
                    result True
                $ \exception ->
                    result $ throw exception
            `shouldReturn` True

        it "can execute a registered action" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Right _ <-registerAction Local checkTCP
                    (Right _) <- executeRegistered Local $ key checkTCP
                    -- confirm results were written
                    Right results' <- allResultsFrom Local
                                                     (convert (0::Int))
                    result $ length results'
                $ \exception ->
                    result $ throw exception
            `shouldReturn` 1

        it "will fail to execute a non-registered action" $ do
            testAgentNL $ \result -> do
                catchAny $ do
                    Right () <- unregisterAction Local (key checkTCP)
                    Left (ActionNotFound _) <- executeRegistered Local $ key checkTCP
                    result True -- no match failure
                $ \exception ->
                    result $ throw exception
            `shouldReturn` True

        it "can execute a supplied action" $ do
            testAgent $ \result -> do
                catchAny $ do
                    (Right nr) <- executeAction Local checkTCP
                    let Just (NagiosResult _ OK) = extract nr
                    -- confirm results were written
                    Right results' <- actionResultsFrom Local
                                                        "localhost:53"
                                                        (convert (0::Int))
                    result $ length results'
                $ \exception ->
                    result $ throw exception
            `shouldReturn` 1

        it "will invoke any configured listeners" $ do
            testAgent $ \result -> do
                catchAny $ do
                    -- generate some results to hear about
                    forM_ [0..2] $ \_ ->
                        executeAction Local checkTCP
                    -- make sure he's been listening
                    threadDelay 1000
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
                    nodeid <- thisNodeId
                    getSelfPid >>= register listenerName
                    let matcher = $(mkClosure 'matchRemoteHostName) (nodeid, listenerName)
                    Right () <- addListener Local matcher
                    threadDelay 10000
                    Right _ <- executeAction Local checkTCP
                    nr <- expect :: Agent Result
                    let Just (NagiosResult _ status) = extract nr
                    result status
                $ \exception ->
                    result $ throw exception
            `shouldReturn` OK

        it "will persist its listeners across restarts" $ do
            -- recover with state in memory after Supervisor restarts
            result' <- testAgent $ \result -> do
                catchAny $ do
                    nodeid <- thisNodeId
                    getSelfPid >>= register listenerName
                    let matcher = $(mkClosure 'matchRemoteHostName) (nodeid, listenerName)
                    Right () <- addListener Local matcher
                    threadDelay 10000
                    Just expid <- whereis $ execServer^.name
                    liftProcess $ kill expid "testing"
                    threadDelay 10000
                    Right _ <- executeAction Local checkTCP
                    nr <- expect :: Agent Result
                    let Just (NagiosResult _ status) = extract nr
                    result status
                $ \exception ->
                    result $ throw exception

            -- recovery with state from disk when launching new agent
            result'' <- testAgentNoSetup $ \result -> do
                catchAny $ do
                    getSelfPid >>= register listenerName
                    Right _ <- executeAction Local checkTCP
                    nr <- expect :: Agent Result
                    let Just (NagiosResult _ status) = extract nr
                    result status
                $ \exception ->
                    result $ throw exception

            return (result' == result'')
            `shouldReturn` True

        it "it won't deliver a routed Action for an unknown context or zone" $ do
            testAgentNL $ \result -> do
                catchAny $ do
                    Left (ECallFailed RoutingFailed) <- executeAction (Route [Context "unkown"] [def])
                                            checkTCP

                    Left (ECallFailed RoutingFailed) <- executeAction (Route [def] [Zone "unkown"])
                                            checkTCP


                    result $ True -- no exceptions
                $ \exception ->
                    result $ throw exception
            `shouldReturn` True



testAgent ma = testRunAgent setup appConfig appPlugins ma

testAgentNoSetup ma = testRunAgent nosetup appConfig appPlugins ma


testAgentNL ma = testRunAgent setup appConfigNL appPlugins ma


-- for testing - useful to throw an exception if we "never" get the value we're expecting
texpect :: (MonadProcess m, Monad m) => forall a. NFSerializable a => m a
texpect = do
    gotit <- expectTimeout 10000 -- 100ms may as well be never
    case gotit of
        Nothing -> error "Timed out in test expect"
        Just v -> return v

checkTCP = CheckTCP  "localhost" 53


listenerName :: String
listenerName = "listener:test"

testActionListener :: Agent [Listener]
testActionListener = do
    startListener sname loop
    nodeid <- thisNodeId
    return [matchAction (\c -> _checktcpHost c == "localhost") nodeid sname]
  where
    sname = "ExecSpecActionListener"
    loop count = do
        receiveWait
            [ match $ \ (_ :: Result) -> do
                  loop $ count + 1
            , match $ \ ("ask-result-count" :: String, pid :: ProcessId) ->
                  send pid count
            ]


testResultListener :: Agent [Listener]
testResultListener = do
    startListener sname loop
    nodeid <- thisNodeId
    return [matchResult (const True)
                        (\ (NagiosResult _ r) -> r == OK)
                        nodeid
                        sname
           ]
  where
    sname = "ExecSpecResultListener"
    loop count = do
        receiveWait
            [ match $ \ (_ :: Result) ->
                  loop $ count + 1
            , match $ \ ("ask-result-count" :: String, pid :: ProcessId) ->
                  send pid count
            ]

startListener sname loop = do
    whereis sname >>= maybe startIt (return . const ())
  where startIt = do
            apid <- liftProcess $ spawnLocal $ loop (0::Int)
            Process.register sname apid

testDef :: NagiosConfig -> PluginDef
testDef conf = definePlugin "ExecSpec"
                            conf
                            ((++) <$> testActionListener <*> testResultListener)
                            []
                            (return ())


-- use a local config here because we are wiring up our own test listener
appConfig :: AgentConfig
appConfig = Helper.appConfig & appendRemoteTable __remoteTable
      {-& agentConfig.minLogLevel .~ LevelDebug-}

appPlugins :: PluginSet
appPlugins =
    pluginSet $ do
        addPlugin $ Nagios.pluginDef def {
            -- override default plugin-specific config
            _nagiosPluginsPath = "/usr/lib/nagios/plugins"
        }
        addPlugin $ testDef def

appConfigNL = appConfig & minLogLevel .~ LevelOther ""
