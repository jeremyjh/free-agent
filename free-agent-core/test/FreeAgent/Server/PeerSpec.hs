{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Server.PeerSpec (main, spec) where

import           AgentPrelude
import qualified Prelude as P
import           FreeAgent.Core
import           FreeAgent.Process
import           FreeAgent.Server.Executive as Exec
import           FreeAgent.Lenses
import           FreeAgent.Plugins.Nagios as Nagios
import           FreeAgent.Server (runAgentServers)
import           FreeAgent.Server.Peer

import           FreeAgent.TestHelper hiding (appConfig, setup)
import qualified FreeAgent.TestHelper as Helper

import qualified Data.Set as Set


import           Control.Concurrent.Lifted

import           Control.Exception
import           Test.Hspec
import System.Process (system)

matchRemoteHostName :: (NodeId, String) -> Listener
matchRemoteHostName (nodeid, name') = matchAction (\c -> _checktcpHost c == "localhost") nodeid name'
remotable ['matchRemoteHostName]

main :: IO ()
main = hspec spec

listenerName :: String
listenerName = "listener:test"

spec :: Spec
spec =
    describe "FreeAgent.Peer" $ do
        it "is started by Core supervisor" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Just _ <- whereis $ peerServer^.name
                    result True
                $ \exception ->
                    result $ throw exception
            `shouldReturn` True

        it "can find services offered on peers" $ do
        -- one giant mega-spec b/c there is so much overhead in spinning up
        -- the peer swarm
            testAgent $ \result -> do
                catchAny $ do
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
                            count <- queryPeerCount
                            when (count < 3) (threadDelay 10000 >> waitFor3)
                    waitFor3

                    -- it "can count peers"
                    count <- queryPeerCount

                    -- it "can query for a Set of matching Peers"
                    peers <- queryPeerServers Exec.serverName (Set.fromList [def] )
                                                              (Set.fromList [Zone "TX"] )

                    -- it "can register remote listeners"
                    let tx:_ = Set.toList peers
                    getSelfPid >>= register listenerName
                    nodeid <- thisNodeId
                    let matcher = $(mkClosure 'matchRemoteHostName) (nodeid, listenerName)
                    Right () <- addListener (Remote tx) matcher
                    threadDelay 10000

                    -- it "can route an action to a remote Node"
                    Right res <- executeAction (Route [def] [Zone "TX"])
                                               checkTCP

                    nr <- expect :: Agent Result
                    let Just (NagiosResult _ status) = extract nr

                    let aname = res ^.to summary.resultOf.to key

                    -- we're done, tell the two "remotes" to exit
                    all3 <- queryPeerServers serverName (Set.fromList[def])
                                                        (Set.fromList[def])
                    forM_ all3 $ \peer' -> do
                        mpid <- resolve (peer', ("waiter"::String))
                        case mpid of
                            Nothing -> return ()
                            Just pid -> do
                                send pid ("waithere" :: String)
                    threadDelay 10000

                    result (count, length peers,aname, status)
                $ \exception ->
                    result $ throw exception
            `shouldReturn` (3, 1, "localhost:53", OK)



testAgent ma = testRunAgent setup appConfig appPlugins ma

setup :: IO ()
setup = do
    void $ system ("rm -rf " ++ (convert $ appConfig^.dbPath))
    void $ system ("rm -rf " ++ (convert $ appConfig2^.dbPath))
    void $ system ("rm -rf " ++ (convert $ appConfigTX^.dbPath))

appConfig :: AgentConfig
appConfig = Helper.appConfig & appendRemoteTable __remoteTable
                             & dbPath .~ "/tmp/leveltestp"
      {-& minLogLevel .~ LevelDebug-}

appConfig2 :: AgentConfig
appConfig2 = appConfig
      & dbPath .~ "/tmp/leveltest2"
      & nodePort .~ "9092"
      & peerNodeSeeds .~ ["127.0.0.1:3546"]
      {-& minLogLevel .~ LevelInfo-}

appConfigTX :: AgentConfig
appConfigTX = appConfig
      & dbPath .~ "/tmp/fa-test-tx"
      & nodePort .~ "9093"
      & peerNodeSeeds .~ ["127.0.0.1:3546"]
      & zones .~ Set.fromList [def, Zone "TX"]
      {-& minLogLevel .~ LevelDebug-}

checkTCP = CheckTCP  "localhost" 53
