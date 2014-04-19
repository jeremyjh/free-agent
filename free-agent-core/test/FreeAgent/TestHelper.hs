{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module FreeAgent.TestHelper
    ( appConfig
    , appPlugins
    , testRunAgent
    , setup
    , nosetup
    , checkTCP
    , texpect
    , TestAction(..)
    , testAction
    , slowTestAction
    ) where

import FreeAgent
import FreeAgent.Lenses
import FreeAgent.Server
import FreeAgent.Process
-- import all your plugins here
import FreeAgent.Plugins.Nagios as Nagios
import Control.Concurrent.Lifted (threadDelay)
import System.Process (system)
import Data.Time.Clock (getCurrentTime)

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentConfig
appConfig = def & dbPath .~ "/tmp/leveltest"
                {-& minLogLevel .~ LevelDebug-}

appPlugins :: PluginSet
appPlugins =
    pluginSet $ do
        addPlugin $ Nagios.pluginDef def {
            -- override default plugin-specific config
            nagiosPluginsPath = "/usr/lib/nagios/plugins"
        }

-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testRunAgent :: NFData a => IO () -> AgentConfig -> PluginSet -> ((a -> Agent ()) -> Agent ()) -> IO a
testRunAgent doSetup config' plugins' ma = do
    doSetup
    result <- newEmptyMVar
    runAgentServers config' plugins' (ma (putMVar result))
    threadDelay 15000 -- so we dont get open port errors
    catchAnyDeep (takeMVar result) $ \ e -> throwIO e

setup :: IO ()
setup = void $ system ("rm -rf " ++ (convert $ appConfig^.dbPath))

nosetup :: IO ()
nosetup = return ()

-- for testing - useful to throw an exception if we "never" get the value we're expecting
texpect :: (MonadProcess m, Monad m) => NFSerializable a => m a
texpect = do
    gotit <- expectTimeout 50000 -- 100ms may as well be never
    case gotit of
        Nothing -> error "Timed out in test expect"
        Just v -> return v

data TestAction = TestAction Text Int
    deriving (Show, Eq, Typeable, Generic)

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

-- common test fixture
checkTCP :: CheckTCP
checkTCP = CheckTCP  "localhost" 53

testAction :: TestAction
testAction = TestAction "test action" 0

-- test with delay
slowTestAction :: TestAction
slowTestAction = TestAction "slow test action" 10000

deriveSerializers ''TestAction
deriveSerializers ''TestResult
