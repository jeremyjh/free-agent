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

import FreeAgent.Core
import FreeAgent.AgentPrelude
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Server
import FreeAgent.Process

import FreeAgent.Fixtures (testPluginDef)

import Data.Map.Strict as Map

import Control.Concurrent.Lifted (threadDelay, fork)
import Control.Exception (throw)
import System.Process (system)
import System.IO.Unsafe (unsafePerformIO)

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentConfig
appConfig = def & dbPath .~ "memory"
                {-& minLogLevel .~ LevelDebug-}

appPlugins :: PluginSet
appPlugins =
    pluginSet $ do
        addPlugin $ testPluginDef

-- helper for running agent and getting results out of
-- the Process through partially applied putMVar
testRunAgent :: NFData a
             => IO () -- ^ setup action
             -> Int -- ^ milliseconds delay
             -> AgentConfig -- ^ can use 'appConfig'
             -> PluginSet -- ^ can use  'appPlugins'
             -> Agent a -- monad action to execute
             -> IO a
testRunAgent doSetup timeout config' plugins' ma = do
    doSetup
    result <- returnFromAgent timeout ma (runAgentServers config' plugins')
    threadDelay 15000 -- so we dont get open port errors
    return result

-- | Given (t, c, p) provide a lookup t with which to cache an
-- 'AgentContext' created from 'AgentConfig' c and 'PluginSet' p
type CachedContext = (Text, AgentConfig, PluginSet)

quickRunAgent :: NFData a => Int -> CachedContext -> Agent a -> IO a
quickRunAgent timeout cached@(name', _ , _) ma = do
    contexts' <- readMVar cachedContexts
    context' <- case Map.lookup name' contexts' of
                    Just context' -> return context'
                    Nothing -> createContext cached
    returnFromAgent timeout ma (void . forkAgent context')

returnFromAgent :: (NFData a, MonadBaseControl IO io)
                => Int -> Agent a -> (Agent () -> io ()) -> io a
returnFromAgent timeout ma agentFn = do
    resultMV <- newEmptyMVar
    void . fork $ agentFn $ do
        eresult <- tryAnyDeep ma
        case eresult of
            Right result -> putMVar resultMV result
            Left exception -> putMVar resultMV (throw exception)
    !result <- waitMVar resultMV
    return result
  where
    waitMVar mv = loop 0
      where loop lapsed
              | lapsed < timeout =
                  tryTakeMVar mv >>= maybe (delay >> loop (lapsed + 1))
                                           return
              | otherwise = error "Execution of Agent test timed-out."
            delay = threadDelay 1000

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
setup =
    case appConfig ^. dbPath of
        "memory" -> return ()
        _ ->
            void $ system ("rm -rf " ++ (convert $ appConfig ^. dbPath))

setupConfig :: AgentConfig -> IO ()
setupConfig config' = void $ system ("rm -rf " ++ (convert $ config' ^. dbPath))

nosetup :: IO ()
nosetup = return ()

-- for testing - useful to throw an exception if we "never" get the value we're expecting
texpect :: (MonadProcess m, Monad m) => NFSerializable a => m a
texpect = do
    gotit <- expectTimeout 500000 -- 500ms may as well be never
    case gotit of
        Nothing -> error "Timed out in test expect"
        Just v -> return v
