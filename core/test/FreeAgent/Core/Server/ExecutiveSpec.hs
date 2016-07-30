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

import           FreeAgent.TestHelper hiding (appConfig, appPlugins, testAgent)
import qualified FreeAgent.TestHelper as Helper
import           FreeAgent.Fixtures

import           Control.Concurrent.Lifted
import           FreeAgent.Process as Process
import           System.Process (system)


matchRemoteHostName :: (NodeId, String) -> Listener
matchRemoteHostName (nodeid, name') = actionListener (\(TestCheckTCP host' _) -> host' == "localhost") nodeid name'
remotable ['matchRemoteHostName]

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll_ setup $ afterAll_ cleanup $ do
    describe "Basic executive operations" $ do

        it "is started by Core supervisor" $ testAgent (
             do Just _ <- resolve Exec.serverName
                return True
            ) `shouldReturn` True

        it "can execute a stored action" $ testAgent (
             do Right _ <- callServ $ StoreAction (Action checkTCP)
                execCountResults (key checkTCP)
            ) `shouldReturn` 1

        it "can store multiple actions at once" $ testAgent (
             do Right _ <- callServ $ StoreActions [Action checkTCP, Action testAction]
                execCountResults (key testAction)
            ) `shouldReturn` 1

        it "will persist stored actions across restarts" $ (
          do
             Right _ <- testAgentDb (callServ $ StoreAction (Action checkTCP))
             closeContext "executiveDb"
             threadDelay 100000
             testAgentDb $
               do Right _ <- executeStored $ key checkTCP
                  -- confirm results were written
                  Right results' <- findResultsSince (convert (0::Int))
                  return $ length results'
          ) `shouldReturn` 1

        it "will not update a newer action" $ testAgent (
             do let willWork = Action $ (defaultShellCommand "willWork")
                                                            {shellCommand = "ls"}
                    wontWork = Action $ (defaultShellCommand "wontWork")
                                                            {shellCommand = "ls"
                                                            ,shellFailCodes = [0]}
                beforeTime <- getCurrentTime
                Right _ <- callServ $ StoreAction willWork
                Right _ <- callServ $ StoreNewerAction wontWork beforeTime

                Right _ <- executeStored $ key willWork
                return True
            ) `shouldReturn` True

        it "can execute a stored action asynchronously" $ testAgent (
             do Right _ <- callServ $ StoreAction (Action testAction)
                Right _ <- castServ $ ExecuteStored (key testAction)

                threadDelay 10000
                -- confirm results were written
                Right results' <- findResultsSince (convert (0::Int))
                return $ length results'
            ) `shouldReturn` 4

        it "will fail to execute a non-registered action" $ testAgentNL (
             do Right () <- callServ $ RemoveAction (key testAction)
                Right (Left (ActionNotFound _)) <- callServ $ ExecuteStored (key testAction)
                return True -- no match failure
            ) `shouldReturn` True

        it "can execute a supplied action" $ testAgent (
             do (Right nr) <- executeAction checkTCP
                Just (NagiosResult _ OK) <- return $ extractResult nr
                -- confirm results were written

                Right results' <- findActionResultsSince (key checkTCP)
                                                         (convert (0::Int))
                return $ length results'
            ) `shouldReturn` 2

    describe "Listener notifications" $ do
        it "will invoke any configured listeners" $ testAgent (
                -- generate some results to hear about
             do forM_ [0..2] $ \_ ->
                    void $ executeAction checkTCP
                -- make sure he's been listening
                threadDelay 1000
                pid <- getSelfPid
                nsend "ExecSpecActionListener" ("ask-result-count" :: String, pid)
                actionCount <- texpect :: Agent Int
                nsend "ExecSpecResultListener" ("ask-result-count" :: String, pid)
                resultCount <- texpect :: Agent Int
                return (actionCount, resultCount)
            ) `shouldReturn` (5,5)

        it "can subscribe listeners at runtime" $ testAgent (
             do registerListener
                executeExtractResult checkTCP
            ) `shouldReturn` OK

        it "will persist its listeners across restarts" $ do
            -- recover with state in memory after Supervisor restarts
            result' <- testAgentDb $ do
                    registerListener
                    Just expid <- whereis Exec.serverName
                    liftP $ kill expid "testing"
                    threadDelay 10000
                    executeExtractResult checkTCP

            closeContext "executiveDb"
            threadDelay 100000
            -- recovery with state from disk when launching new agent
            result'' <- testAgentDb $ do
                getSelfPid >>= register listenerName
                executeExtractResult checkTCP
            return (result' == result'')
            `shouldReturn` True

    describe "Routing features supplied by Peer" $
        it "it won't deliver a routed Action for an unknown context or zone" $ testAgentNL (
             do Left (ECallFailed RoutingFailed)
                    <- withTarget (Route [Context "unkown"] [def]) $
                          executeAction checkTCP

                Left (ECallFailed RoutingFailed)
                    <- withTarget (Route [def] [Zone "unkown"]) $
                          executeAction checkTCP
                return  True -- no exceptions
            ) `shouldReturn` True


-- use a local config here because we are wiring up our own test listener
appConfig :: AgentConfig
appConfig = Helper.appConfig & appendRemoteTable __remoteTable
      {-& agentConfig.minLogLevel .~ LevelDebug-}

appPlugins :: PluginSet
appPlugins =
    pluginSet $ do
        addPlugin testDef
        addPlugin testPluginDef

setup = void $ system ("rm -rf " ++ "/tmp/db")

cleanup = do closeContext "executive"
             closeContext "executiveNL"
             closeContext "executiveDb"

testAgent :: NFData a => Agent a -> IO a
testAgent = quickRunAgent 500 ("executive", appConfig, appPlugins)


testAgentDb :: NFData a => Agent a -> IO a
testAgentDb = quickRunAgent 500 ("executiveDb"
                                , appConfig & dbPath .~ "/tmp/db"
                                            & nodePort .~ "9190"
                                , appPlugins)

testAgentNL :: NFData a => Agent a -> IO a
testAgentNL = quickRunAgent 1000 ("executiveNL", appConfigNL, appPlugins)

listenerName :: String
listenerName = "listener:test"

testActionListener :: Agent [Listener]
testActionListener = do
    startListener sname loop
    nodeid <- thisNodeId
    return [actionListener (\(TestCheckTCP host' _) -> host' == "localhost") nodeid sname]
  where
    sname = "ExecSpecActionListener"
    loop count =
        receiveWait
            [ match $ \ (_ :: Result) ->
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
    loop count =
        receiveWait
            [ match $ \ (_ :: Result) ->
                  loop $ count + 1
            , match $ \ ("ask-result-count" :: String, pid :: ProcessId) ->
                  send pid count
            ]

startListener sname loop =
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


appConfigNL = appConfig & minLogLevel .~ LevelOther ""

execCountResults actionkey =
    do now <- getCurrentTime
       Right _ <- executeStored actionkey
       -- confirm results were written
       Right results' <- findActionResultsSince actionkey now
       return $ length results'

registerListener :: Agent ()
registerListener =
  do nodeid <- thisNodeId
     getSelfPid >>= register listenerName
     let matcher = $(mkClosure 'matchRemoteHostName) (nodeid, listenerName)
     Right () <- castServ (AddListener matcher)
     threadDelay 10000

executeExtractResult action' =
  do Right _ <- executeAction action'
     nr <- texpect :: Agent Result
     let Just (NagiosResult _ status) = extractResult nr
     return status
