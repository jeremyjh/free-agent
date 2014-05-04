{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Server.ScheduleSpec (main, spec) where

import           AgentPrelude
import qualified Prelude as P
import           FreeAgent.Process
import           FreeAgent.Lenses
import           FreeAgent.Server.Schedule as Schedule
import           FreeAgent.Server.Executive.History (allResultsFrom)
import           FreeAgent.Server.Executive (registerAction)

import           FreeAgent.TestHelper hiding (appConfig)
import qualified FreeAgent.TestHelper as Helper
import           FreeAgent.Fixtures

import           Control.Concurrent.Lifted (threadDelay)
import           Control.Distributed.Process.Platform.Timer (Tick(..))
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do --parallel $
    describe "Basic scheduler operations" $ do
        it "is started by Core supervisor" $ do
            testAgent $ do
                Just _ <- resolve scheduleServer
                return True
            `shouldReturn` True

        it "can schedule and find an event" $ do
            testAgent $ do
                Right () <- schedule Local $
                                     Event (key testAction) "@hourly" Never
                Right _ <- findEvent Local (key testAction)
                return True
            `shouldReturn` True

        it "won't schedule a bogus cron format" $ do
            testAgent $ do
                Right () <- schedule Local $
                                     Event (key testAction)
                                           "whatever man"
                                           Never
                Left (EventNotFound _) <- findEvent Local (key testAction)
                return ()
            `shouldThrow` errorCall "Unable to parse cron formatted literal: whatever man"

        it "won't find an absent event" $ do
            testAgent $ do
                Left (EventNotFound key') <- findEvent Local "not here"
                return key'
            `shouldReturn` "not here"

        it "can remove an event" $ do
            testAgent $ do
                Right () <- schedule Local $
                                     Event ("will delete") "@hourly" Never

                Right _ <- findEvent Local "will delete"
                Right () <- unschedule Local "will delete"
                Left (EventNotFound key') <- findEvent Local "will delete"
                return key'
            `shouldReturn` "will delete"

        it "executes a cron scheduled action" $ do
            testAgent $ do
                Right () <- registerAction Local testAction
                Right () <- schedule Local $ Event (key testAction)
                                                   "* * * * *"
                                                   Never
                send scheduleServer Tick
                threadDelay 10000
                Right results' <- allResultsFrom Local
                                                 (convert (0::Int))
                return (length results')
            `shouldReturn` 1

testAgent ma = quickRunAgent ("4122"
                             , appConfig & nodePort .~ "4122"
                             , appPlugins
                             ) ma

{-testAgentNL ma = testRunAgent setup appConfigNL appPlugins ma-}

appConfig :: AgentConfig
appConfig = Helper.appConfig
      {-& minLogLevel .~ LevelDebug-}
{-appConfigNL = Helper.appConfig & minLogLevel .~ LevelOther ""-}
