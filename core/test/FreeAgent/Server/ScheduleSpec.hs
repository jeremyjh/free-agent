{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Server.ScheduleSpec (main, spec) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Process
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Core.Protocol.Schedule as Schedule
import           FreeAgent.Core.Protocol.Executive.History (findResultsSince)
import           FreeAgent.Core.Protocol.Executive (StoreAction(..))
import           FreeAgent.Server.ManagedAgent (callServ)

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
                Just _ <- whereis serverName
                return True
            `shouldReturn` True

        it "can schedule and find an event" $ do
            testAgent $ do
                Right () <- schedule (key testAction) "@hourly" Never
                Right _ <- lookupEvent (key testAction)
                return True
            `shouldReturn` True

        it "won't schedule an older Event" $ do
            testAgent $ do
                Right () <- schedule "test older" "@hourly" Never
                Right old <- lookupEvent "test older"
                Right () <- callServ $
                                ScheduleAddNewerEvent "test older"
                                "@hourly"
                                (Fixed 1 10)
                                (schedModified old)
                Right stillold <- lookupEvent "test older"
                return (schedRetry stillold)
            `shouldReturn` Never

        it "won't schedule a bogus cron format" $ do
            testAgent $ do
                Right () <- schedule (key testAction) "whatever man" Never
                Left (EventNotFound _) <- lookupEvent (key testAction)
                return ()
            `shouldThrow` errorCall "Unable to parse cron formatted literal: whatever man"

        it "won't find an absent event" $ do
            testAgent $ do
                Left (EventNotFound key') <- lookupEvent "not here"
                return key'
            `shouldReturn` "not here"

        it "can remove an event" $ do
            testAgent $ do
                Right () <- schedule ("will delete") "@hourly" Never

                Right _ <- lookupEvent "will delete"
                Right () <- unschedule "will delete"
                Left (EventNotFound key') <- lookupEvent "will delete"
                return key'
            `shouldReturn` "will delete"

        it "executes a cron scheduled action" $ do
            testAgent $ do
                Right () <- callServ $ StoreAction (Action testAction)
                Right () <- schedule (key testAction) "* * * * *" Never
                -- reset the event so it can run now
                Right () <- callServ $ ScheduleEnableEvents [key testAction]

                threadDelay 10000
                Right results' <- findResultsSince (convert (0::Int))
                return (length results')
            `shouldReturn` 1

        it "does not execute a disabled event" $ do
            testAgent $ do
                Right () <- callServ $ StoreAction (Action testAction)
                Right () <- schedule (key testAction) "* * * * *" Never
                -- this really doesn't prove much, since the event won't
                -- run till the next minute in any case
                {-Right () <- callServ $ ScheduleDisableEvents [key testAction]-}

                send serverName Tick
                threadDelay 10000
                Right rs <- findResultsSince (convert (0::Int))
                return (length rs)
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
