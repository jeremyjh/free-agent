{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults#-}


module FreeAgent.Server.ExecutiveSpec (main, spec) where

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
import           FreeAgent.Fixtures

import           Control.Concurrent.Lifted
import           FreeAgent.Process as Process


matchRemoteHostName :: (NodeId, String) -> Listener
matchRemoteHostName (nodeid, name') = matchAction (\c -> _checktcpHost c == "localhost") nodeid name'
remotable ['matchRemoteHostName]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Basic executive operations" $ do

        it "is started by Core supervisor" $ do
            testAgent $ do
                Just _ <- resolve execServer
                return True
            `shouldReturn` True

        it "can execute a registered action" $ do
            testAgent $ do
                Right _ <-registerAction Local checkTCP
                (Right _) <- executeRegistered Local $ key checkTCP
                -- confirm results were written
                Right results' <- allResultsFrom Local
                                                 (convert (0::Int))
                return $ length results'
            `shouldReturn` 1

        it "can execute a registered action asynchronously" $ do
            testAgent $ do
                Right _ <-registerAction Local testAction
                Right _ <- executeRegisteredAsync Local $ key testAction
                threadDelay 1000
                -- confirm results were written
                Right results' <- allResultsFrom Local
                                                 (convert (0::Int))
                return $ length results'
            `shouldReturn` 2

        it "will fail to execute a non-registered action" $ do
            testAgentNL $ do
                Right () <- unregisterAction Local (key testAction)
                Left (ActionNotFound _) <- executeRegistered Local $ key testAction
                return True -- no match failure
            `shouldReturn` True

        it "can execute a supplied action" $ do
            testAgent $ do
                (Right nr) <- executeAction Local checkTCP
                let Just (NagiosResult _ OK) = extract nr
                -- confirm results were written
                Right results' <- actionResultsFrom Local
                                                    (key checkTCP)
                                                    (convert (0::Int))
                return $ length results'
            `shouldReturn` 2

    describe "Listener notifications" $ do
        it "will invoke any configured listeners" $ do
            testAgent $ do
                -- generate some results to hear about
                forM_ [0..2] $ \_ ->
                    executeAction Local checkTCP
                -- make sure he's been listening
                threadDelay 1000
                pid <- getSelfPid
                nsend "ExecSpecActionListener" ("ask-result-count" :: String, pid)
                actionCount <- texpect :: Agent Int
                nsend "ExecSpecResultListener" ("ask-result-count" :: String, pid)
                resultCount <- texpect :: Agent Int
                return (actionCount, resultCount)
            `shouldReturn` (5,5)

        it "can subscribe listeners at runtime" $ do
            testAgent $ do
                nodeid <- thisNodeId
                getSelfPid >>= register listenerName
                let matcher = $(mkClosure 'matchRemoteHostName) (nodeid, listenerName)
                Right () <- addListener Local matcher
                threadDelay 10000
                Right _ <- executeAction Local checkTCP
                nr <- texpect :: Agent Result
                let Just (NagiosResult _ status) = extract nr
                return status
            `shouldReturn` OK

        it "will persist its listeners across restarts" $ do
            -- recover with state in memory after Supervisor restarts
            result' <- testAgent $ do
                    nodeid <- thisNodeId
                    getSelfPid >>= register listenerName
                    let matcher = $(mkClosure 'matchRemoteHostName) (nodeid, listenerName)
                    Right () <- addListener Local matcher
                    threadDelay 10000
                    Just expid <- whereis $ execServer^.name
                    liftProcess $ kill expid "testing"
                    threadDelay 10000
                    Right _ <- executeAction Local checkTCP
                    nr <- texpect :: Agent Result
                    let Just (NagiosResult _ status) = extract nr
                    return status

            -- recovery with state from disk when launching new agent
            result'' <- testAgent $ do
                nr <- liftProcess $ do
                    getSelfPid >>= register listenerName
                    Right _ <- executeAction Local checkTCP
                    texpect :: Process Result
                let Just (NagiosResult _ status) = extract nr
                return status
            return (result' == result'')
            `shouldReturn` True

    describe "Routing features supplied by Peer" $ do
        it "it won't deliver a routed Action for an unknown context or zone" $ do
            testAgentNL $ do
                Left (ECallFailed RoutingFailed)
                    <- executeAction (Route [Context "unkown"] [def])
                                     checkTCP

                Left (ECallFailed RoutingFailed)
                    <- executeAction (Route [def] [Zone "unkown"])
                                     checkTCP
                return  True -- no exceptions
            `shouldReturn` True



testAgent ma = quickRunAgent ("4120"
                             , appConfig & nodePort .~ "4120"
                             , appPlugins
                             ) ma

{-testAgentNoSetup ma = testRunAgent nosetup appConfig appPlugins ma-}


testAgentNL ma = testRunAgent setup appConfigNL appPlugins ma

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
            nagiosPluginsPath = "/usr/lib/nagios/plugins"
        }
        addPlugin $ testDef def

appConfigNL = appConfig & minLogLevel .~ LevelOther ""
