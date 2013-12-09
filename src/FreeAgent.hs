{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgent
    ( module X
    , freeAgentMain
    ) where


import           FreeAgent.Prelude as X
import           FreeAgent.Types as X
import           FreeAgent.Action as X
import           FreeAgent.Core as X
import           FreeAgent.Executive as Exec
import           Control.Distributed.Process.Lifted as Process


freeAgentMain:: AgentContext -> IO ()
freeAgentMain ctxt = do
    runAgent ctxt $ do
        mainPid <- getSelfPid
        Process.register "main" mainPid
        execPid <- Exec.init
        putStrLn $ "Exec process awaiting commands on " ++ (tshow execPid)
        putStrLn $ "Main process waiting for 'terminate' on " ++ (tshow mainPid)
        "terminate" <- expect :: Agent String
        return ()
    return ()

