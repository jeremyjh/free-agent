{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgent
    ( module X
    , freeAgentMain
    ) where


import           FreeAgent.AgentPrelude as X
import           FreeAgent.Core.Internal.Types as X
import           FreeAgent.Core.Action as X
import           FreeAgent.Core as X
import           FreeAgent.Process as Process


freeAgentMain:: AgentConfig -> PluginSet -> IO ()
freeAgentMain config' plugins' = do
    runAgent config' plugins' $ do
        mainPid <- getSelfPid
        Process.register "main" mainPid
        putStrLn $ "Main process waiting for 'terminate' on " ++ tshow mainPid
        "terminate" <- expect :: Agent String
        return ()
    return ()

