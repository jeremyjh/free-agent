{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module FreeAgent.Fixtures
    ( checkTCP
    , TestCheckTCP(..)
    , TestAction(..)
    , NagiosResult(..)
    , CommandResult(..)
    , testAction
    , testFailAction
    , slowTestAction
    , testPluginDef
    , summaryNow
    ) where

import FreeAgent.Core
import FreeAgent.Core.Action
import FreeAgent.AgentPrelude
import FreeAgent.Core.Lenses

import Control.Concurrent.Lifted (threadDelay)

data TestAction = TestAction Text Int
    deriving (Show, Eq, Typeable, Generic)

data TestFailAction = TestFailAction Text
    deriving (Show, Eq, Typeable, Generic)

instance Stashable TestFailAction where
    key (TestFailAction text') = text'

data TestResult = TestResult ResultSummary
    deriving (Show, Eq, Typeable, Generic)

instance Stashable TestAction where
    key (TestAction text' _) = text'

instance Stashable TestResult where
    key (TestResult summ) = key summ

instance Extractable TestResult

instance Resulting TestResult where
    summary (TestResult summ) = summ


instance Extractable TestAction

instance Runnable TestAction TestResult where
    exec ta@(TestAction text' delay) = do
        threadDelay delay
        time' <- getCurrentTime
        return $ Right $ TestResult (ResultSummary time' text' (toAction ta))

    execWith action' _ = do
        exec action'

instance Extractable TestFailAction

instance Runnable TestFailAction TestResult where
    exec (TestFailAction text') = return $ Left (GeneralFailure (text'))

    execWith action' _ = do
        summ <- resultNow "onFailure called" action'
        return $ Right (TestResult summ)

data TestCheckTCP = TestCheckTCP Text Int
    deriving (Show, Eq, Typeable, Generic)

instance Stashable TestCheckTCP where
    key (TestCheckTCP host' port')= host' ++ ":" ++ tshow port'

data CommandResult = OK | Warning | Critical | Unknown
    deriving (Show, Eq, Typeable, Generic)
deriveSerializers ''CommandResult

data NagiosResult = NagiosResult
    { _nagresResultSummary :: ResultSummary
    , _nagresResult :: CommandResult
    } deriving (Show, Eq, Typeable, Generic)

makeFields ''NagiosResult
deriveSerializers ''NagiosResult

instance Stashable NagiosResult where
    key = key . summary

instance Resulting NagiosResult where
    summary (NagiosResult s _) = s

instance Extractable NagiosResult
instance Extractable TestCheckTCP

instance Runnable TestCheckTCP NagiosResult where
    exec action' = do
        summ <- summaryNow "Test succeed."  action'
        return $ Right (NagiosResult summ OK)

-- common test fixture
checkTCP :: TestCheckTCP
checkTCP = TestCheckTCP "localhost" 631

testAction :: TestAction
testAction = TestAction "test action" 0

testFailAction :: Text -> TestFailAction
testFailAction text' = TestFailAction text'

-- test with delay
slowTestAction :: TestAction
slowTestAction = TestAction "slow test action" 10000

testPluginDef :: PluginDef
testPluginDef = definePlugin "FixturesPlugin" () (return []) [] $
 do registerAction (actionType :: Proxy TestAction )
    registerAction (actionType :: Proxy TestFailAction)
    registerAction (actionType :: Proxy TestCheckTCP)

summaryNow :: (Runnable action b, MonadIO io)
           => Text -> action -> io ResultSummary
summaryNow msg resultOf' = do
    now <- getCurrentTime
    return $ ResultSummary now msg (toAction resultOf')

deriveSerializers ''TestAction
deriveSerializers ''TestFailAction
deriveSerializers ''TestResult
deriveSerializers ''TestCheckTCP