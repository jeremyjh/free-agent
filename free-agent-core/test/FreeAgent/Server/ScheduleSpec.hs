{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Server.ScheduleSpec (main, spec) where

import           AgentPrelude
import qualified Prelude as P
import           FreeAgent.Process
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Server.Schedule as Schedule
import           FreeAgent.Server.Executive.History (allResultsFrom)
import           FreeAgent.Server.Executive (RegisterAction(..))
import           FreeAgent.Server.Peer (request)

import           FreeAgent.TestHelper hiding (appConfig)
import qualified FreeAgent.TestHelper as Helper
import           FreeAgent.Fixtures

import           Control.Concurrent.Lifted (threadDelay)
import           Control.Distributed.Process.Platform.Timer (Tick(..))
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =  --parallel $
    describe "Basic scheduler operations" $ do
        it "is started by Core supervisor" $ do
            testAgent $ do
                Just _ <- resolve scheduleServer
                return True
            `shouldReturn` True

        it "can schedule and find an event" $ do
            testAgent $ do
                Right () <- schedule $ Event (key testAction) "@hourly" Never
                Right _ <- findEvent (key testAction)
                return True
            `shouldReturn` True

        it "won't schedule a bogus cron format" $ do
            testAgent $ do
                Right () <- schedule $ Event (key testAction)
                                             "whatever man"
                                             Never
                Left (EventNotFound _) <- findEvent (key testAction)
                return ()
            `shouldThrow` errorCall "Unable to parse cron formatted literal: whatever man"

        it "won't find an absent event" $ do
            testAgent $ do
                Left (EventNotFound key') <- findEvent "not here"
                return key'
            `shouldReturn` "not here"

        it "can remove an event" $ do
            testAgent $ do
                Right () <- schedule $ Event ("will delete") "@hourly" Never

                Right _ <- findEvent "will delete"
                Right () <- unschedule "will delete"
                Left (EventNotFound key') <- findEvent "will delete"
                return key'
            `shouldReturn` "will delete"

        it "executes a cron scheduled action" $ do
            testAgent $ do
                Right () <- request $ RegisterAction (Action testAction)
                Right () <- schedule $ Event (key testAction)
                                            "* * * * *"
                                            Never
                send scheduleServer Tick
                threadDelay 10000
                Right results' <- allResultsFrom (convert (0::Int))
                return (length results')
            `shouldReturn` 1

testAgent ma = quickRunAgent 500
                             ("4122"
                             , appConfig & nodePort .~ "4122"
                             , appPlugins
                             ) ma

{-testAgentNL ma = testRunAgent setup appConfigNL appPlugins ma-}

appConfig :: AgentConfig
appConfig = Helper.appConfig
      {-& minLogLevel .~ LevelDebug-}
{-appConfigNL = Helper.appConfig & minLogLevel .~ LevelOther ""-}
