{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving          #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, RecordWildCards, TemplateHaskell, TypeFamilies  #-}


module FreeAgent.Server.Executive.History
    ( writeResult
    , allResultsFrom
    , actionResultsFrom
    , historyServerImpl
    , defaultHistoryServer
    , serverName
    , HistoryBackend
    ) where

import FreeAgent.AgentPrelude
import FreeAgent.Process (getSelfPid, say)
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Database.AcidState
import FreeAgent.Server.ManagedAgent

import Control.Monad.State            (StateT)
import Data.Binary                    (Binary)
import qualified Data.CircularList as CL
import Data.CircularList (CList)


-- -----------------------------
-- Types
-- -----------------------------

-- | defines back-end implementation
data HistoryBackend st = HistoryBackend {
      doInit              :: Agent st
    , doWriteResult       :: Result -> HistoryAgent st ()
    , doAllResultsFrom    :: UTCTime -> HistoryAgentET st [Result]
    , doActionResultsFrom :: Key -> UTCTime -> HistoryAgentET st [Result]
    , doShutdown          :: ShutdownHandler st
}

type HistoryAgent st = StateT st Agent
type HistoryAgentET st = EitherT HistoryFail (StateT st Agent)

data HistoryFail = HCallFailed CallFail
        deriving (Show, Eq, Typeable, Generic)
instance Binary HistoryFail

instance Convertible CallFail HistoryFail where
    safeConvert cf = return (HCallFailed cf)

data HistoryCommand = WriteResult Result
                    | AllResultsFrom UTCTime
                    | ActionResultsFrom Key UTCTime
        deriving (Show, Eq, Typeable, Generic)

instance Binary HistoryCommand

-- -----------------------------
-- API
-- -----------------------------

writeResult        :: (MonadAgent agent)
                   => Result -> agent (Either CallFail ())
writeResult result' =
    castTarget serverName $ WriteResult result'

allResultsFrom :: (MonadAgent agent)
                   => UTCTime -> agent (Either HistoryFail [Result])
allResultsFrom = callHistory . AllResultsFrom

actionResultsFrom:: (MonadAgent agent)
                   => Key -> UTCTime -> agent (Either HistoryFail [Result])
actionResultsFrom key' = callHistory . ActionResultsFrom key'


serverName :: String
serverName = "agent:executive:history"

callHistory :: (NFSerializable a, MonadAgent agent)
            => HistoryCommand -> agent (Either HistoryFail a)
callHistory command = do
    eresult <- callTarget serverName command
    case eresult of
        Right result' -> return result'
        Left cf -> return (Left $ HCallFailed cf)

historyServerImpl :: HistoryBackend st -> AgentServer
historyServerImpl HistoryBackend{..} =
    defineServer serverName
                 doInit
                 defaultProcess {
                     apiHandlers =
                     [ agentCastHandler $
                           \cmd -> case cmd of
                               WriteResult r -> doWriteResult r
                               _ -> $(err "illegal pattern match")

                     , agentRpcHandlerET $
                           \cmd -> case cmd of
                               AllResultsFrom time -> doAllResultsFrom time
                               ActionResultsFrom key' time -> doActionResultsFrom key' time
                               _ -> $(err "illegal pattern match")
                     ]
                  , shutdownHandler = \ (AgentState _ st) reason ->
                       do pid <- getSelfPid
                          doShutdown st reason
                          say $ "Exec History server " ++ show pid ++ " shutting down."
                 }

-- -----------------------------
-- Default Implementation
-- Using circular list and AcidState
-- -----------------------------

data HistoryPersist = HistoryPersist {_historyResults :: CList Result}
        deriving (Typeable)

data HistoryState = HistoryState { _historyAcid         :: AcidState HistoryPersist
                                 , _historySessionCount :: Int
                                 }

makeFields ''HistoryPersist

makeFields ''HistoryState


fetchAllFrom  :: UTCTime -> Query HistoryPersist [Result]
fetchAllFrom time =
    takeWhile (\r -> r ^. to summary.timestamp >= time)
                  <$> CL.rightElements  <$> view results

insertResult :: Result -> Update HistoryPersist ()
insertResult r =
 do results' <- use results
    if CL.size results' < 5000
        then results %= CL.insertR r
        else results %= CL.update r . CL.rotL

$(makeAcidic ''HistoryPersist ['insertResult, 'fetchAllFrom])

defaultBackend :: HistoryBackend HistoryState
defaultBackend = HistoryBackend {
      doInit = do
        acid' <- openOrGetDb "agent-executive-history"
                                  (HistoryPersist CL.empty) def
        return $ HistoryState acid' 0
    , doWriteResult = \ rs ->
        do void $ update $ InsertResult rs
           sessionCount %= (+1)
    , doAllResultsFrom = query . FetchAllFrom
    , doActionResultsFrom = \ key' time -> do
        results' <- query (FetchAllFrom time)
        return $ filter (\r -> r ^. to summary.resultOf.to key == key')
                        results'
    , doShutdown = \ (HistoryState _ count) _ ->
        say $ "Exec history wrote event count of: " ++ show count
}

defaultHistoryServer :: AgentServer
defaultHistoryServer = historyServerImpl defaultBackend

deriveSafeStore ''HistoryPersist
instance NFData HistoryCommand where rnf = genericRnf
instance NFData HistoryFail where rnf = genericRnf