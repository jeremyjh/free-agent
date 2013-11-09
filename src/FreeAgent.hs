{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module FreeAgent
    ( module FreeAgent.Prelude
    , module FreeAgent.Types
    , module FreeAgent.Action
    , module FreeAgent.Core
    , freeAgentMain
    ) where


import           FreeAgent.Prelude
import           FreeAgent.Types
import           FreeAgent.Action
import           FreeAgent.Core
import           Database.LevelDB.Higher


freeAgentMain:: AgentConfig -> IO ()
freeAgentMain _ = do
    runCreateLevelDB "/tmp/leveltest10" "hello" $ do
        put "first" "firstvalue"
        put "second" "secondvalue"

    return ()

