{-# LANGUAGE TypeFamilies #-}
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
    ) where

import FreeAgent.Core
import FreeAgent.AgentPrelude
import FreeAgent.Core.Lenses

import Control.Concurrent.Lifted (threadDelay)

data TestAction = TestAction Text Int
    deriving (Show, Eq, Typeable, Generic)

data TestFailAction = TestFailAction Text
    deriving (Show, Eq, Typeable, Generic)

instance Stashable TestFailAction where
    key (TestFailAction text') = text'

data TestResult = TestResult Key
    deriving (Show, Eq, Typeable, Generic)

instance Stashable TestAction where
    key (TestAction text' _) = text'

instance Runnable TestAction where
    type RunnableResult TestAction = TestResult
    exec ta@(TestAction text' delay) = do
        threadDelay delay
        Right <$> resultNow (TestResult (key ta)) text' (toAction ta)

instance Runnable TestFailAction where
    exec (TestFailAction text') = return $ Left (GeneralFailure text')

    execWith action' _ =
        Right <$> resultNow (TestResult $ key action') "onFailure called" action'

data TestCheckTCP = TestCheckTCP Text Int
    deriving (Show, Eq, Typeable, Generic)

instance Stashable TestCheckTCP where
    key (TestCheckTCP host' port')= host' ++ ":" ++ tshow port'

data CommandResult = OK | Warning | Critical | Unknown
    deriving (Show, Eq, Typeable, Generic)
deriveSerializers ''CommandResult

data NagiosResult = NagiosResult Key CommandResult
    deriving (Show, Eq, Typeable, Generic)

makeFields ''NagiosResult
deriveSerializers ''NagiosResult

instance Runnable TestCheckTCP where
    type RunnableResult TestCheckTCP = NagiosResult
    exec action' =
        Right <$> resultNow (NagiosResult (key action') OK) "Test succeed."  action'

-- common test fixture
checkTCP :: TestCheckTCP
checkTCP = TestCheckTCP "localhost" 631

testAction :: TestAction
testAction = TestAction "test action" 0

testFailAction :: Text -> TestFailAction
testFailAction = TestFailAction

-- test with delay
slowTestAction :: TestAction
slowTestAction = TestAction "slow test action" 10000

testPluginDef :: PluginDef
testPluginDef = definePlugin "FixturesPlugin" () (return []) [] $
 do registerAction (actionType :: Proxy TestAction )
    registerAction (actionType :: Proxy TestFailAction)
    registerAction (actionType :: Proxy TestCheckTCP)

deriveSerializers ''TestAction
deriveSerializers ''TestFailAction
deriveSerializers ''TestResult
deriveSerializers ''TestCheckTCP
