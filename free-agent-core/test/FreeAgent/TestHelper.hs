{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgent.TestHelper
    ( appConfig
    , appPlugins
    , testRunAgent
    , setup
    , nosetup
    , checkTCP
    , texpect
    ) where

import FreeAgent
import FreeAgent.Lenses
import FreeAgent.Server
import FreeAgent.Process
-- import all your plugins here
import FreeAgent.Plugins.Nagios as Nagios
import Control.Concurrent.Lifted (threadDelay)
import System.Process (system)

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

-- common test fixture
checkTCP :: CheckTCP
checkTCP = CheckTCP  "localhost" 53

-- for testing - useful to throw an exception if we "never" get the value we're expecting
texpect :: (MonadProcess m, Monad m) => NFSerializable a => m a
texpect = do
    gotit <- expectTimeout 50000 -- 100ms may as well be never
    case gotit of
        Nothing -> error "Timed out in test expect"
        Just v -> return v
