{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module FreeAgent.TestHelper
    ( appConfig
    , appPlugins
    , testRunAgent
    , quickRunAgent
    , setup
    , nosetup
    , texpect
    ) where

import FreeAgent
import FreeAgent.Lenses
import FreeAgent.Server
import FreeAgent.Process

import FreeAgent.Fixtures (testPluginDef)

import Data.Map.Strict as Map

import FreeAgent.Plugins.Nagios as Nagios
import Control.Concurrent.Lifted (threadDelay, fork)
import Control.Exception (throw)
import System.Process (system)
import System.IO.Unsafe (unsafePerformIO)

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentConfig
appConfig = def & dbPath .~ "/tmp/leveltest"
                {-& minLogLevel .~ LevelDebug-}

appPlugins :: PluginSet
appPlugins =
    pluginSet $ do
        addPlugin $ testPluginDef
        addPlugin $ Nagios.pluginDef def {
            -- override default plugin-specific config
            nagiosPluginsPath = "/usr/lib/nagios/plugins"
        }

-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testRunAgent :: NFData a => IO () -> AgentConfig -> PluginSet -> Agent a -> IO a
testRunAgent doSetup config' plugins' ma = do
    doSetup
    result <- returnFromAgent ma (runAgentServers config' plugins')
    threadDelay 15000 -- so we dont get open port errors
    return result

type CachedContext = (Text, AgentConfig, PluginSet)

quickRunAgent :: NFData a => CachedContext -> Agent a -> IO a
quickRunAgent cached@(name', _ , _) ma = do
    contexts' <- readMVar cachedContexts
    context' <- case Map.lookup name' contexts' of
                    Just context' -> return context'
                    Nothing -> createContext cached
    returnFromAgent ma (void . forkAgent context')

returnFromAgent :: (NFData a, MonadBase IO io)
                => Agent a -> (Agent () -> io ()) -> io a
returnFromAgent ma agentFn = do
    resultMV <- newEmptyMVar
    agentFn $ do
        eresult <- tryAnyDeep ma
        case eresult of
            Right result -> putMVar resultMV result
            Left exception -> putMVar resultMV (throw exception)
    !result <- takeMVar resultMV
    return result

cachedContexts :: MVar (Map Text AgentContext)
cachedContexts = unsafePerformIO (newMVar mempty)
{-# NOINLINE cachedContexts #-}

createContext :: CachedContext -> IO AgentContext
createContext (name', config', plugins') = do
    setupConfig config'
    contextsMap <- takeMVar cachedContexts
    case Map.lookup name' contextsMap of
        Just context' -> do
            -- | lost race to create this particular context
            putMVar cachedContexts contextsMap
            return context'
        Nothing -> do
            void . fork $ runAgentServers config' plugins' $ do
                context' <- askContext
                putMVar cachedContexts (Map.insert name' context' contextsMap)
                "the end" <- expect :: Agent String
                return ()
            contexts' <- readMVar cachedContexts
            return $ fromMaybe (error "couldn't create Context!")
                               (Map.lookup name' contexts')


setup :: IO ()
setup = void $ system ("rm -rf " ++ (convert $ appConfig^.dbPath))

setupConfig :: AgentConfig -> IO ()
setupConfig config' = void $ system ("rm -rf " ++ (convert $ config'^.dbPath))

nosetup :: IO ()
nosetup = return ()

-- for testing - useful to throw an exception if we "never" get the value we're expecting
texpect :: (MonadProcess m, Monad m) => NFSerializable a => m a
texpect = do
    gotit <- expectTimeout 50000 -- 100ms may as well be never
    case gotit of
        Nothing -> error "Timed out in test expect"
        Just v -> return v
