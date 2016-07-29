{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Server.PeerSpec (main, spec) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core
import           FreeAgent.Core.Lenses
import           FreeAgent.Server (runAgentServers)
import qualified FreeAgent.Core.Protocol.Executive as Exec

import           FreeAgent.TestHelper hiding (appConfig, testAgent)
import qualified FreeAgent.TestHelper as Helper
import           FreeAgent.Fixtures

import qualified Data.Set as Set


import           Control.Concurrent.Lifted

import           Test.Hspec

matchRemoteHostName :: (NodeId, String) -> Listener
matchRemoteHostName (nodeid, name') = actionListener (\(TestCheckTCP host' _) -> host' == "localhost") nodeid name'
remotable ['matchRemoteHostName]

main :: IO ()
main = hspec $ afterAll_ cleanup spec

listenerName :: String
listenerName = "listener:test"

spec :: Spec
spec =
    describe "FreeAgent.Peer" $ do
        it "can find services offered on peers" $ testAgent (
        -- one giant mega-spec b/c there is so much overhead in spinning up
        -- the peer swarm
                -- create a couple "remotes"
              do let waiter = do getSelfPid >>= register "waiter"
                                 "waithere" <- expect :: Agent String
                                 return ()

                 void $ fork $ liftIO $
                     runAgentServers appConfig2 appPlugins waiter

                 void $ fork $ liftIO $
                     runAgentServers appConfigTX appPlugins waiter


                 -- wait for swarm to stabilize
                 let waitFor3 = do
                         Right count <- queryPeerCount
                         when (count < 3) (threadDelay 5000 >> waitFor3)
                 waitFor3

                 -- it "can count peers"
                 Right count <- queryPeerCount

                 -- it "can query for a Set of matching Peers"
                 Right peers <- queryPeerServers Exec.serverName
                                                 (Set.fromList [def])
                                                 (Set.fromList [Zone "TX"])

                 -- it "can register remote listeners"
                 let tx:_ = Set.toList peers
                 getSelfPid >>= register listenerName
                 nodeid <- thisNodeId
                 let matcher = $(mkClosure 'matchRemoteHostName) (nodeid, listenerName)
                 Right () <- withTarget (Remote tx) $
                                 castServ (AddListener matcher)
                 threadDelay 1000

                 -- it "can route an action to a remote Node"
                 Right _ <- withTarget (Route [def] [Zone "TX"]) $
                                 executeAction checkTCP

                 nr <- texpect :: Agent Result
                 let Just (NagiosResult _ status) = extractResult nr

                 let aname = key $ resultResultOf nr

                 -- we're done, tell the two "remotes" to exit
                 Right all3 <- queryPeerServers peerServerName (Set.fromList[def])
                                                               (Set.fromList[def])
                 forM_ all3 $ \peer' -> do
                     mpid <- resolve (peer', "waiter"::String)
                     case mpid of
                         Nothing -> return ()
                         Just pid ->
                             send pid ("waithere" :: String)
                 threadDelay 1000

                 return (count, Set.size peers,aname, status)
            ) `shouldReturn` (3, 1, key checkTCP, OK)

cleanup = closeContext "peer"

testAgent :: NFData a => Agent a -> IO a
testAgent = quickRunAgent 2000 ("peer", appConfig & nodePort .~ "9090", appPlugins)

appConfig :: AgentConfig
appConfig = Helper.appConfig & appendRemoteTable __remoteTable
      {-& minLogLevel .~ LevelDebug-}

appConfig2 :: AgentConfig
appConfig2 = appConfig
      & peerNodeSeeds .~ ["127.0.0.1:9090"]
      {-& minLogLevel .~ LevelInfo-}

appConfigTX :: AgentConfig
appConfigTX = appConfig
      & peerNodeSeeds .~ ["127.0.0.1:9090"]
      & zones .~ Set.fromList [def, Zone "TX"]
      {-& minLogLevel .~ LevelDebug-}
