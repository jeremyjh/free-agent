{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import           FreeAgent.Core
import           FreeAgent.Process
import           FreeAgent.Server.Peer (castServer, callServer, CallFail)
import qualified FreeAgent.Server.Peer as Peer
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

writeResult        :: (MonadAgent m)
                   => Target -> Result -> m (Either CallFail ())
writeResult target = castServer serverName target . WriteResult

allResultsFrom :: (MonadAgent m)
                   => Target -> UTCTime -> m (Either HistoryFail [Result])
allResultsFrom target = callHistory target . AllResultsFrom

actionResultsFrom:: (MonadAgent m)
                   => Target -> Key -> UTCTime -> m (Either HistoryFail [Result])
actionResultsFrom target key' = callHistory target . ActionResultsFrom key'


serverName :: String
serverName = "agent:executive:history"

callHistory :: (NFSerializable a, MonadAgent m)
            => Target -> HistoryCommand -> m (Either HistoryFail a)
callHistory target command = do
    eresult <- callServer serverName target command
    case eresult of
        Right result' -> return result'
        Left cf -> return (Left $ HCallFailed cf)

historyServerImpl :: HistoryBackend st -> AgentServer
historyServerImpl backend@HistoryBackend{..} = AgentServer serverName child
  where
    init ctxt = do
        state' <- withAgent ctxt doInit
        serve state' initHistory historyProcess
      where
        initHistory state' = do
            pid <- getSelfPid
            Peer.registerServer (historyServerImpl backend) pid
            return $ InitOk (AgentState ctxt state') Infinity

        historyProcess = defaultProcess {
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

    child ctxt = do
        initChild <- toChildStart $ init ctxt

        return ChildSpec {
              childKey     = ""
            , childType    = Worker
            , childRestart = Permanent
            , childStop    = TerminateTimeout (Delay $ milliSeconds 10)
            , childStart   = initChild
            , childRegName = Just $ LocalName serverName
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
fetchAllFrom time = view results >>=
    return . (takeWhile (\r -> r^.to summary.timestamp >= time))

insertResult :: Result -> Update HistoryPersist ()
insertResult r = results %= (r :)

$(makeAcidic ''HistoryPersist ['insertResult, 'fetchAllFrom])

defaultBackend :: HistoryBackend HistoryState
defaultBackend = HistoryBackend {
      doInit = do
        Just acid' <- openOrGetDb "agent-executive-history"
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
