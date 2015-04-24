{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults#-}


module FreeAgent.Core.Server.ExecutiveSpec (main, spec) where

import           Test.Hspec

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Core
import           FreeAgent.Core.Action.ShellCommand
import           FreeAgent.Core.Protocol.Executive as Exec

import           FreeAgent.TestHelper hiding (appConfig, appPlugins)
import qualified FreeAgent.TestHelper as Helper
import           FreeAgent.Fixtures

import           Control.Concurrent.Lifted
import           FreeAgent.Process as Process


matchRemoteHostName :: (NodeId, String) -> Listener
matchRemoteHostName (nodeid, name') = actionListener (\(TestCheckTCP host' _) -> host' == "localhost") nodeid name'
remotable ['matchRemoteHostName]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Basic executive operations" $ do

        it "is started by Core supervisor" $ do
            testAgent $ do
                Just _ <- resolve Exec.serverName
                return True
            `shouldReturn` True

        it "can execute a stored action" $ do
            testAgent $ do
                Right _ <- callServ $ StoreAction (Action checkTCP)
                (Right (_ :: NagiosResult ) ) <- executeStored $ key checkTCP
                -- confirm results were written
                Right results' <- findResultsSince (convert (0::Int))
                return $ length results'
            `shouldReturn` 1

        it "will not update a newer action" $ do
            testAgent $ do
                let willWork = Action $ (defaultShellCommand "willWork")
                                                            {shellCommand = "ls"}
                    wontWork = Action $ (defaultShellCommand "wontWork")
                                                            {shellCommand = "ls"
                                                            ,shellFailCodes = [0]}
                beforeTime <- getCurrentTime
                Right _ <- callServ $ StoreAction willWork
                Right _ <- callServ $ StoreNewerAction wontWork beforeTime

                Right (_ :: ShellResult)<- executeStored $ key willWork
                return True
            `shouldReturn` True

        it "can execute a stored action asynchronously" $ do
            testAgent $ do
                Right _ <- callServ $ StoreAction (Action testAction)
                Right _ <- castServ $ ExecuteStored (key testAction)

                threadDelay 10000
                -- confirm results were written
                Right results' <- findResultsSince (convert (0::Int))
                return $ length results'
            `shouldReturn` 3

        it "will fail to execute a non-registered action" $ do
            testAgentNL $ do
                Right () <- callServ $ RemoveAction (key testAction)
                Right (Left (ActionNotFound _)) <- callServ $ ExecuteStored (key testAction)
                return True -- no match failure
            `shouldReturn` True

        it "can execute a supplied action" $ do
            testAgent $ do
                (Right nr) <- executeAction checkTCP
                Just (NagiosResult _ OK) <- return $ extractResult nr
                -- confirm results were written

                Right results' <- findActionResultsSince (key checkTCP)
                                                         (convert (0::Int))
                return $ length results'
            `shouldReturn` 2

    describe "Listener notifications" $ do
        it "will invoke any configured listeners" $ do
            testAgent $ do
                -- generate some results to hear about
                forM_ [0..2] $ \_ ->
                    void $ executeAction checkTCP
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
                Right () <- castServ (AddListener matcher)
                threadDelay 10000
                Right _ <- executeAction checkTCP
                nr <- texpect :: Agent Result
                let Just (NagiosResult _ status) = extractResult nr
                return status
            `shouldReturn` OK

        it "will persist its listeners across restarts" $ do
            -- recover with state in memory after Supervisor restarts
            result' <- testAgent $ do
                    nodeid <- thisNodeId
                    getSelfPid >>= register listenerName
                    let matcher = $(mkClosure 'matchRemoteHostName) (nodeid, listenerName)
                    Right () <- castServ (AddListener matcher)
                    threadDelay 10000
                    Just expid <- whereis $ Exec.serverName
                    liftP $ kill expid "testing"
                    threadDelay 10000
                    Right _ <- executeAction checkTCP
                    nr <- texpect :: Agent Result
                    let Just (NagiosResult _ status) = extractResult nr
                    return status

            -- recovery with state from disk when launching new agent
            result'' <- testAgent $ do
                getSelfPid >>= register listenerName
                Right _ <- executeAction checkTCP
                nr <- texpect :: Agent Result
                let Just (NagiosResult _ status) = extractResult nr
                return status
            return (result' == result'')
            `shouldReturn` True

    describe "Routing features supplied by Peer" $ do
        it "it won't deliver a routed Action for an unknown context or zone" $ do
            testAgentNL $ do
                Left (ECallFailed RoutingFailed)
                    <- withTarget (Route [Context "unkown"] [def]) $
                          executeAction checkTCP

                Left (ECallFailed RoutingFailed)
                    <- withTarget (Route [def] [Zone "unkown"]) $
                          executeAction checkTCP
                return  True -- no exceptions
            `shouldReturn` True



testAgent ma = quickRunAgent 500
                             ("4120"
                             , appConfig & nodePort .~ "4120"
                             , appPlugins
                             ) ma

{-testAgentNoSetup ma = testRunAgent nosetup appConfig appPlugins ma-}


testAgentNL ma = testRunAgent setup 1000 appConfigNL appPlugins ma

listenerName :: String
listenerName = "listener:test"

testActionListener :: Agent [Listener]
testActionListener = do
    startListener sname loop
    nodeid <- thisNodeId
    return [actionListener (\(TestCheckTCP host' _) -> host' == "localhost") nodeid sname]
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
    return [resultListener (const True)
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
            apid <- liftP $ spawnLocal $ loop (0::Int)
            Process.register sname apid

testDef :: PluginDef
testDef = definePlugin "ExecSpec"
                       ("" :: Text)
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
        addPlugin testDef
        addPlugin testPluginDef

appConfigNL = appConfig & minLogLevel .~ LevelOther ""