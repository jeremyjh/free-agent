{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


module FreeAgent.Server.Executive.History
    ( writeResult
    , allResultsFrom
    , actionResultsFrom
    , historyServerImpl
    , defaultHistoryServer
    , serverName
    , HistoryBackend
    ) where

import           AgentPrelude
import           FreeAgent.Lenses
import           FreeAgent.Server.Peer (castServer, callServer, CallFail)
import           FreeAgent.Database.AcidState
import           FreeAgent.Process.ManagedAgent

import           Data.Time.Clock (UTCTime)
import           Data.Binary (Binary)
import           Control.Monad.State (StateT)
import           Control.DeepSeq.TH (deriveNFData)
import Control.Error (EitherT)

-- -----------------------------
-- Types
-- -----------------------------

-- | defines back-end implementation
data HistoryBackend st = HistoryBackend {
      doInit              :: Agent st
    , doWriteResult       :: Result -> HistoryAgent st ()
    , doAllResultsFrom    :: UTCTime -> HistoryAgentET st [Result]
    , doActionResultsFrom :: Key -> UTCTime -> HistoryAgentET st [Result]
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

writeResult        :: (MonadProcess m)
                   => Target -> Result -> m (Either CallFail ())
writeResult target = castServer serverName target . WriteResult

allResultsFrom :: (MonadProcess m)
                   => Target -> UTCTime -> m (Either HistoryFail [Result])
allResultsFrom target = callHistory target . AllResultsFrom

actionResultsFrom:: (MonadProcess m)
                   => Target -> Key -> UTCTime -> m (Either HistoryFail [Result])
actionResultsFrom target key' = callHistory target . ActionResultsFrom key'


serverName :: String
serverName = "agent:executive:history"

callHistory :: (NFSerializable a, MonadProcess m)
            => Target -> HistoryCommand -> m (Either HistoryFail a)
callHistory target command = do
    eresult <- callServer serverName target command
    case eresult of
        Right result' -> return result'
        Left cf -> return (Left $ HCallFailed cf)

historyServerImpl :: HistoryBackend st -> AgentServer
historyServerImpl backend@HistoryBackend{..} =
    defineServer serverName
                 doInit
                 defaultProcess {
                     apiHandlers =
                     [ handleCast $ agentCastHandler $
                           \cmd -> case cmd of
                               WriteResult r -> doWriteResult r
                               _ -> $(err "illegal pattern match")

                     , handleRpcChan $ agentCallHandlerET $
                           \cmd -> case cmd of
                               AllResultsFrom time -> doAllResultsFrom time
                               ActionResultsFrom key' time -> doActionResultsFrom key' time
                               _ -> $(err "illegal pattern match")
                     ]
                 }

-- -----------------------------
-- Default Implementation
-- Using circular list and AcidState
-- -----------------------------

data HistoryPersist = HistoryPersist {_historyResults :: [Result]}
        deriving (Typeable)

makeFields ''HistoryPersist

data HistoryState = HistoryState {_historyAcid :: AcidState HistoryPersist}

fetchAllFrom  :: UTCTime -> Query HistoryPersist [Result]
fetchAllFrom time =
    takeWhile (\r -> r^.to summary.timestamp >= time) <$> view results

insertResult :: Result -> Update HistoryPersist ()
insertResult r = results %= (r :)

$(makeAcidic ''HistoryPersist ['insertResult, 'fetchAllFrom])

defaultBackend :: HistoryBackend HistoryState
defaultBackend = HistoryBackend {
      doInit = do
        acid' <- openOrGetDb "agent-executive-history"
                                  (HistoryPersist []) def
        return $ HistoryState acid'
    , doWriteResult = update . InsertResult
    , doAllResultsFrom = query . FetchAllFrom
    , doActionResultsFrom = \ key' time -> do
        results' <- query (FetchAllFrom time)
        return $ filter (\r -> r^.to summary.resultOf.to key == key')
                        results'

}

defaultHistoryServer :: AgentServer
defaultHistoryServer = historyServerImpl defaultBackend

makeFields ''HistoryState
deriveSafeStore ''HistoryPersist
deriveNFData ''HistoryCommand
deriveNFData ''HistoryFail
