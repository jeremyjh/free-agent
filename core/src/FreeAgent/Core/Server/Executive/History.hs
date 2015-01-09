{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving          #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, RecordWildCards, TemplateHaskell, TypeFamilies  #-}


module FreeAgent.Core.Server.Executive.History
    ( historyServerImpl
    , defaultHistoryServer
    , serverName
    , HistoryImpl
    ) where

import FreeAgent.AgentPrelude
import FreeAgent.Core
import FreeAgent.Core.Lenses
import FreeAgent.Core.Protocol.Executive.History (serverName)
import FreeAgent.Database.AcidState
import FreeAgent.Server.ManagedAgent


import qualified Data.CircularList as CL
import Data.CircularList (CList)
import Control.Monad.State (StateT)



historyServerImpl :: HistoryImpl st -> AgentServer
historyServerImpl impl@HistoryImpl{..} =
    defineServer serverName
                 doInit
                 defaultProcess {
                     apiHandlers =
                     [   registerCall impl (Proxy :: Proxy FindResults)
                     ,   registerCast impl (Proxy :: Proxy WriteResult)
                     ]
                  , shutdownHandler = \ (AgentState stats _ _) reason ->
                       do pid <- getSelfPid
                          say $ "History Stats: " ++ show stats
                          say $ "Exec History server " ++ show pid
                                                       ++ " shutting down: " ++ show reason
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

--TODO: really want to parameterize over the monad as well, this still
--needs work and thought
type instance ProtoT WriteResult st a = StateT st Agent a
type instance ProtoT FindResults st a = StateT st Agent a

historyImpl :: HistoryImpl HistoryState
historyImpl = HistoryImpl doInit' castWriteResult' callFindResults'
  where
    doInit' =
     do acid' <- openOrGetDb "agent-executive-history"
                             (HistoryPersist CL.empty) def
        return $ HistoryState acid' 0

    castWriteResult' (WriteResult rs) =
        do void $ update $ InsertResult rs
           sessionCount %= (+1)

    callFindResults' (AllResultsSince time) = query $ FetchAllFrom time
    callFindResults' (ActionResultsSince key' time) =
     do results' <- query (FetchAllFrom time)
        return $ filter (\r -> r ^. to summary.resultOf.to key == key')
                        results'

defaultHistoryServer :: AgentServer
defaultHistoryServer = historyServerImpl historyImpl

deriveSafeStore ''HistoryPersist
