{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module FreeAgent.PeerSpec (main, spec) where

import           AgentPrelude
import qualified Prelude as P
import qualified AppConfig as Config
import           FreeAgent.Core
import           FreeAgent.Process
import           FreeAgent.Server.Executive
import           FreeAgent.Lenses
import           FreeAgent.Plugins.Nagios as Nagios
import           FreeAgent.Server (runAgentServers)
import           FreeAgent.Server.Peer
import           FreeAgent.Server.Executive (execServer)

import qualified Data.Set as Set


import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.Lifted

import           Control.Exception
import           Test.Hspec
import           FreeAgent

matchRemoteHostName :: ProcessId -> Listener
matchRemoteHostName pid = matchAction (\c -> _checktcpHost c == "localhost") pid
remotable ['matchRemoteHostName]

main :: IO ()
main = hspec spec


spec :: Spec
spec =
    describe "FreeAgent.Peer" $ do
        it "is started by Core supervisor" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Just pid <- whereis $ peerServer^.name
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
                    fork $ liftIO $
                        runAgentServers appConfig2 $ do
                                "waithere" <- expect :: Agent String
                                return ()

                    fork $ liftIO $
                        runAgentServers appConfigTX $ do
                            "waithere" <- expect :: Agent String
                            return ()

                    threadDelay 500000 -- wait for the swarm to stabilize

                    -- it "can count peers"
                    count <- queryPeerCount

                    -- it "can query for a Set of matching Peers"
                    peers <- queryPeerServers execServer (Set.fromList [def] )
                                                         (Set.fromList [Zone "TX"] )

                    -- it "can register remote listeners"
                    let tx:_ = Set.toList peers
                    pid <- getSelfPid
                    let matcher = $(mkClosure 'matchRemoteHostName) pid
                    addRemoteListener (tx, execServer) matcher
                    threadDelay 10000

                    package <- set actions [Action checkTCP] <$> defaultPackage
                                    & fmap (set zones $ Set.fromList [Zone "TX"])

                    -- it "can route a package to a remote Node"
                    (Right res):_<- deliverPackage package

                    nr <- expect :: Agent Result
                    let Just (NagiosResult _ status) = extract nr

                    let aname = res ^.to summary.resultOf.to key

                    result (count, length peers,aname, status)
                $ \exception ->
                    result $ throw exception
            `shouldReturn` (2, 1, "localhost", OK)


-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testAgent :: ((a -> Agent ()) -> Agent ()) -> IO a
testAgent ma = do
    result <- newEmptyMVar
    runAgentServers appConfig (ma (putMVar result))
    threadDelay 2000 -- so we dont get open port errors
    takeMVar result

appConfig :: AgentContext
appConfig = Config.appConfig & appendRemoteTable __remoteTable
      {-& agentConfig.minLogLevel .~ LevelDebug-}

appConfig2 :: AgentContext
appConfig2 = appConfig
      & agentConfig.dbPath .~ "/tmp/leveltest2" -- override Agent config values here!
      & agentConfig.nodePort .~ "9092"
      & agentConfig.peerNodeSeeds .~ ["127.0.0.1:3546"]
      {-& agentConfig.minLogLevel .~ LevelInfo-}

appConfigTX :: AgentContext
appConfigTX = appConfig
      & agentConfig.dbPath .~ "/tmp/fa-test-tx" -- override Agent config values here!
      & agentConfig.nodePort .~ "9093"
      & agentConfig.peerNodeSeeds .~ ["127.0.0.1:3546"]
      & agentConfig.zones .~ Set.fromList [def, Zone "TX"]
      {-& agentConfig.minLogLevel .~ LevelDebug-}

testDef :: AgentConfig -> PluginDef
testDef conf = definePlugin "PeerSpec"
                            ()
                            (return [])
                            (return ())

checkTCP = CheckTCP  "localhost" 53
