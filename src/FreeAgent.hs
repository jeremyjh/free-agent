{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgent
    ( module X
    , freeAgentMain
    ) where


import           AgentPrelude as X
import           FreeAgent.Types as X
import           FreeAgent.Action as X
import           FreeAgent.Core as X
import           FreeAgent.Process as Process


freeAgentMain:: AgentContext -> IO ()
freeAgentMain ctxt = do
    runAgent ctxt $ do
        mainPid <- getSelfPid
        Process.register "main" mainPid
        putStrLn $ "Main process waiting for 'terminate' on " ++ tshow mainPid
        "terminate" <- expect :: Agent String
        return ()
    return ()

