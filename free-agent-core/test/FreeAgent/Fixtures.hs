{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module FreeAgent.Fixtures
    ( checkTCP
    , TestAction(..)
    , testAction
    , testFailAction
    , slowTestAction
    , testPluginDef
    ) where

import FreeAgent
-- import all your plugins here
import FreeAgent.Plugins.Nagios as Nagios
import Control.Concurrent.Lifted (threadDelay)
import Data.Time.Clock (getCurrentTime)

data TestAction = TestAction Text Int
    deriving (Show, Eq, Typeable, Generic)

data TestFailAction = TestFailAction Text
    deriving (Show, Eq, Typeable, Generic)

instance Stashable TestFailAction where
    key (TestFailAction text') = convert text'

data TestResult = TestResult ResultSummary
    deriving (Show, Eq, Typeable, Generic)

instance Stashable TestAction where
    key (TestAction text' _) = convert text'

instance Stashable TestResult where
    key (TestResult summ) = key summ

instance Resulting TestResult where
    summary (TestResult summ) = summ

instance Runnable TestAction TestResult where
    exec ta@(TestAction text' delay) = do
        threadDelay delay
        time' <- liftIO getCurrentTime
        return $ Right $ TestResult (ResultSummary time' text' (toAction ta))

    execWith action' _ = do
        exec action'

instance Runnable TestFailAction TestResult where
    exec (TestFailAction text') = return $ Left (GeneralFailure (text'))

    execWith action' _ = do
        summ <- resultSummaryNow "onFailure called" action'
        return $ Right (TestResult summ)

resultSummaryNow :: (MonadAgent agent, Actionable action result)
          => Text -> action -> agent ResultSummary
resultSummaryNow text' action' = do
    time' <- liftIO getCurrentTime
    return $ ResultSummary time' text' (toAction action')

-- common test fixture
checkTCP :: CheckTCP
checkTCP = CheckTCP  "localhost" 53

testAction :: TestAction
testAction = TestAction "test action" 0

testFailAction :: Text -> TestFailAction
testFailAction text' = TestFailAction text'

-- test with delay
slowTestAction :: TestAction
slowTestAction = TestAction "slow test action" 10000

testPluginDef :: PluginDef
testPluginDef = definePlugin "FixturesPlugin" () (return []) [] $
 do register (actionType :: TestAction)
    register (actionType :: TestFailAction)

deriveSerializers ''TestAction
deriveSerializers ''TestFailAction
deriveSerializers ''TestResult
