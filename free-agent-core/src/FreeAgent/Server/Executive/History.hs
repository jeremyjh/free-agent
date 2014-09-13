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
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Database.AcidState
import FreeAgent.Server.ManagedAgent

import Control.Monad.State            (StateT)
import Data.Binary                    (Binary)


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
    takeWhile (\r -> r ^. to summary.timestamp >= time) <$> view results

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
        return $ filter (\r -> r ^. to summary.resultOf.to key == key')
                        results'

}

defaultHistoryServer :: AgentServer
defaultHistoryServer = historyServerImpl defaultBackend

makeFields ''HistoryState
deriveSafeStore ''HistoryPersist
instance NFData HistoryCommand where rnf = genericRnf
instance NFData HistoryFail where rnf = genericRnf
