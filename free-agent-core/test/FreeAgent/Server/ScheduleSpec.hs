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

import           Control.Exception (throw)
import           Control.Concurrent.Lifted (threadDelay)
import           Control.Distributed.Process.Platform.Timer (Tick(..))
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Basic scheduler operations" $ do
        it "is started by Core supervisor" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Just _ <- resolve scheduleServer
                    result True
                $ \exception ->
                    result $ throw exception
            `shouldReturn` True

        it "can schedule and find an event" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Right () <- schedule Local $
                                         Event (key testAction) "@hourly" Never

                    Right _ <- findEvent Local (key testAction)
                    result True
                $ \exception ->
                    result $ throw exception
            `shouldReturn` True

        it "won't schedule a bogus cron format" $ do
            testAgentNL $ \result -> do
                catchAny $ do
                    Left (CronParseFail _) <- schedule Local $
                                                       Event (key testAction)
                                                             "whatever man"
                                                             Never

                    Left (EventNotFound _) <- findEvent Local (key testAction)
                    result True -- no exception
                $ \exception ->
                    result $ throw exception
            `shouldThrow` anyException

        it "won't find an absent event" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Left (EventNotFound key') <- findEvent Local "not here"
                    result key'
                $ \exception ->
                    result $ throw exception
            `shouldReturn` "not here"

        it "can remove an event" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Right () <- schedule Local $
                                         Event ("will delete") "@hourly" Never

                    Right _ <- findEvent Local "will delete"
                    Right () <- unschedule Local "will delete"
                    Left (EventNotFound key') <- findEvent Local "will delete"
                    result key'
                $ \exception ->
                    result $ throw exception
            `shouldReturn` "will delete"

        it "executes a cron scheduled action" $ do
            testAgent $ \result -> do
                catchAny $ do
                    Right () <- registerAction Local testAction
                    Right () <- schedule Local $ Event (key testAction)
                                                       "* * * * *"
                                                       Never
                    send scheduleServer Tick
                    threadDelay 10000
                    Right results' <- allResultsFrom Local
                                                     (convert (0::Int))
                    result (length results')
                $ \exception ->
                    result $ throw exception
            `shouldReturn` 1

testAgent ma = testRunAgent setup appConfig appPlugins ma

testAgentNL ma = testRunAgent setup appConfigNL appPlugins ma

appConfig :: AgentConfig
appConfig = Helper.appConfig
      {-& minLogLevel .~ LevelDebug-}
appConfigNL = Helper.appConfig & minLogLevel .~ LevelOther ""
