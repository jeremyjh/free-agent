{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Server.PeerSpec (main, spec) where

import           FreeAgent.AgentPrelude
import qualified Prelude as P
import           FreeAgent.Core
import           FreeAgent.Process
import           FreeAgent.Core.Protocol.Executive as Exec
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Server (runAgentServers)
import           FreeAgent.Core.Protocol.Peer
import           FreeAgent.Server.ManagedAgent

import           FreeAgent.TestHelper hiding (appConfig)
import qualified FreeAgent.TestHelper as Helper
import           FreeAgent.Fixtures

import qualified Data.Set as Set


import           Control.Concurrent.Lifted

import           Test.Hspec
import System.Process (system)

matchRemoteHostName :: (NodeId, String) -> Listener
matchRemoteHostName (nodeid, name') = matchAction (\(TestCheckTCP host' _) -> host' == "localhost") nodeid name'
remotable ['matchRemoteHostName]

main :: IO ()
main = hspec spec

listenerName :: String
listenerName = "listener:test"

spec :: Spec
spec =
    describe "FreeAgent.Peer" $ do
        it "is started by Core supervisor" $ do
            testAgent $ do
                Just _ <- whereis $ peerServerName
                return True
            `shouldReturn` True

        it "can find services offered on peers" $ do
        -- one giant mega-spec b/c there is so much overhead in spinning up
        -- the peer swarm
            testAgent $ do
                -- create a couple "remotes"
                void $ fork $ liftIO $
                    runAgentServers appConfig2 appPlugins $ do
                        getSelfPid >>= register "waiter"
                        "waithere" <- expect :: Agent String
                        return ()

                void $ fork $ liftIO $
                    runAgentServers appConfigTX appPlugins $ do
                        getSelfPid >>= register "waiter"
                        "waithere" <- expect :: Agent String
                        return ()


                -- wait for swarm to stabilize
                let waitFor3 = do
                        Right count <- queryPeerCount
                        when (count < 3) (threadDelay 10000 >> waitFor3)
                waitFor3

                -- it "can count peers"
                Right count <- queryPeerCount

                -- it "can query for a Set of matching Peers"
                Right peers <- queryPeerServers Exec.serverName (Set.fromList [def] )
                                                          (Set.fromList [Zone "TX"] )

                -- it "can register remote listeners"
                let tx:_ = Set.toList peers
                getSelfPid >>= register listenerName
                nodeid <- thisNodeId
                let matcher = $(mkClosure 'matchRemoteHostName) (nodeid, listenerName)
                Right () <- withTarget (Remote tx) $
                                castServ (AddListener matcher)
                threadDelay 10000

                -- it "can route an action to a remote Node"
                Right res <- withTarget (Route [def] [Zone "TX"]) $
                                executeAction checkTCP

                nr <- texpect :: Agent Result
                let Just (NagiosResult _ status) = extract nr

                let aname = res  ^. to summary.resultOf.to key

                -- we're done, tell the two "remotes" to exit
                Right all3 <- queryPeerServers serverName (Set.fromList[def])
                                                    (Set.fromList[def])
                forM_ all3 $ \peer' -> do
                    mpid <- resolve (peer', ("waiter"::String))
                    case mpid of
                        Nothing -> return ()
                        Just pid -> do
                            send pid ("waithere" :: String)
                threadDelay 10000

                return (count, length peers,aname, status)
            `shouldReturn` (3, 1, key checkTCP, OK)



testAgent ma = testRunAgent setup 2000 appConfig appPlugins ma

appConfig :: AgentConfig
appConfig = Helper.appConfig & appendRemoteTable __remoteTable
      {-& minLogLevel .~ LevelDebug-}

appConfig2 :: AgentConfig
appConfig2 = appConfig
      & nodePort .~ "9092"
      & peerNodeSeeds .~ ["127.0.0.1:3546"]
      {-& minLogLevel .~ LevelInfo-}

appConfigTX :: AgentConfig
appConfigTX = appConfig
      & nodePort .~ "9093"
      & peerNodeSeeds .~ ["127.0.0.1:3546"]
      & zones .~ Set.fromList [def, Zone "TX"]
      {-& minLogLevel .~ LevelDebug-}
