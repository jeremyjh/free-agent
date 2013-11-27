{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgent
    ( module X
    , freeAgentMain
    ) where


import           FreeAgent.Prelude as X
import           FreeAgent.Types as X
import           FreeAgent.Action as X
import           FreeAgent.Core as X
import           Database.LevelDB.Higher


freeAgentMain:: AgentContext -> IO ()
freeAgentMain _ = do
    runCreateLevelDB "/tmp/leveltest10" "hello" $ do
        put "first" "firstvalue"
        put "second" "secondvalue"

    return ()

