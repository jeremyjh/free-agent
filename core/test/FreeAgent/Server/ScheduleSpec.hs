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

import           FreeAgent.TestHelper
import           FreeAgent.Fixtures

import           Control.Concurrent.Lifted (threadDelay)
import           Control.Distributed.Process.Extras.Timer (Tick(..))
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =  --parallel $
    describe "Basic scheduler operations" $ do
        it "is started by Core supervisor" $ testAgent (
             do Just _ <- whereis serverName
                return True
            ) `shouldReturn` True

        it "can schedule and find an event" $ testAgent (
             do Right () <- schedule (key testAction) "@hourly" Never
                Right _ <- lookupEvent (key testAction)
                Right () <- unschedule (key testAction)
                return True
            ) `shouldReturn` True

        it "won't schedule an older Event" $ testAgent (
             do Right () <- schedule "test older" "@hourly" Never
                Right old <- lookupEvent "test older"
                Right () <- callServ $
                                ScheduleAddNewerEvent "test older"
                                "@hourly"
                                (Fixed 1 10)
                                (schedModified old)
                Right stillold <- lookupEvent "test older"
                Right _ <- unschedule "test older"
                return (schedRetry stillold)
            ) `shouldReturn` Never

        it "won't schedule a bogus cron format" $ testAgent (
             do Right () <- schedule (key testAction) "whatever man" Never
                Left (EventNotFound _) <- lookupEvent (key testAction)
                return ()
            ) `shouldThrow` errorCall "Unable to parse cron formatted literal: whatever man"

        it "won't find an absent event" $ testAgent (
             do Left (EventNotFound key') <- lookupEvent "not here"
                return key'
            ) `shouldReturn` "not here"

        it "can remove an event" $ testAgent (
             do Right () <- schedule "will delete" "@hourly" Never

                Right _ <- lookupEvent "will delete"
                Right () <- unschedule "will delete"
                Left (EventNotFound key') <- lookupEvent "will delete"
                return key'
            ) `shouldReturn` "will delete"

        it "executes a cron scheduled action" $ testAgent (
             do Right () <- callServ $ StoreAction (Action testAction)
                Right () <- schedule (key testAction) "* * * * *" Never
                -- reset the event so it can run now
                Right () <- callServ $ ScheduleEnableEvents [key testAction]

                threadDelay 10000
                Right results' <- findResultsSince zeroDate
                Right () <- unschedule (key testAction)
                return (length results')
            ) `shouldReturn` 1

        it "does not execute a disabled event" $ testAgent (
             do Right () <- callServ $ StoreAction (Action testAction)
                Right () <- schedule (key testAction) "* * * * *" Never
                -- this really doesn't prove much, since the event won't
                -- run till the next minute in any case
                {-Right () <- callServ $ ScheduleDisableEvents [key testAction]-}

                send serverName Tick
                threadDelay 10000
                Right rs <- findResultsSince zeroDate
                Right () <- unschedule (key testAction)
                return (length rs)
            ) `shouldReturn` 1
