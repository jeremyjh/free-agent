{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module FreeAgent.TestHelper
    ( appConfig
    , appPlugins
    , quickRunAgent
    , runAgentPool
    , testAgent
    , closeContext
    , texpect
    ) where

import FreeAgent.Core
import FreeAgent.AgentPrelude
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Server

import FreeAgent.Fixtures (testPluginDef)

import Data.Map.Strict as Map

import Control.Concurrent.Lifted (threadDelay, fork)
import Control.Concurrent.MVar.Lifted
import Control.Exception (throw)
import Control.Exception.Enclosed  (tryAnyDeep)
import System.IO.Unsafe (unsafePerformIO)

-- |App Config Section
-- use record syntax to over-ride default configuration values
appConfig :: AgentConfig
appConfig = def & dbPath .~ "memory"
                {-& minLogLevel .~ LevelInfo-}

appPlugins :: PluginSet
appPlugins =
    pluginSet $ addPlugin testPluginDef

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
                getSelfPid >>= register (convert name')
                "the end" <- expect :: Agent String
                return ()
            contexts' <- readMVar cachedContexts
            return $ fromMaybe (error "couldn't create Context!")
                               (Map.lookup name' contexts')

closeContext :: Text -> IO ()
closeContext name' =
  modifyMVar_ cachedContexts $ \ contexts' ->
  do context' <- case Map.lookup name' contexts' of
                    Just context' -> return context'
                    Nothing -> error "context not found"
     returnFromAgent 1000 sendExit (void . forkAgent context')
     return $ Map.delete name' contexts'
  where sendExit =
          do Just pid <- whereis (convert name')
             send pid ("the end" :: String)

contextPool :: MVar [AgentContext]
contextPool = unsafePerformIO (newMVar mempty)
{-# NOINLINE contextPool #-}

runAgentPool :: NFData a => Int -> Agent a -> IO a
runAgentPool timeout ma =
  do context' <- modifyMVar contextPool $ \ clist ->
                    case clist of
                        [] -> do context' <- newContext
                                 return ([], context')
                        first' : rest -> return (rest, first')
     rv <- returnFromAgent timeout ma (void . forkAgent context')
     modifyMVar_ contextPool $ \ clist -> return $ context' : clist
     return rv
  where
    newContext :: IO AgentContext
    newContext =
      do contextMv <- newEmptyMVar
         void . fork $ runAgentServers appConfig appPlugins $ do
             context' <- askContext
             putMVar contextMv context'
             "the end" <- expect :: Agent String
             return ()
         takeMVar contextMv

-- for testing - useful to throw an exception if we "never" get the value we're expecting
texpect :: (MonadProcess m, Monad m) => NFSerializable a => m a
texpect = do
    gotit <- expectTimeout 500000 -- 500ms may as well be never
    case gotit of
        Nothing -> error "Timed out in test expect"
        Just v -> return v

testAgent :: NFData a => Agent a -> IO a
testAgent = runAgentPool 500
