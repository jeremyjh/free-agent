{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, TypeFamilies  #-}


module FreeAgent.Core.Protocol.Executive.History
    ( writeResult
    , findResultsSince
    , findActionResultsSince
    , serverName
    , HistoryImpl(..)
    , FindResults(..)
    , WriteResult(..)
    ) where

import FreeAgent.AgentPrelude
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Core.Protocol
import FreeAgent.Server.ManagedAgent

import Control.Monad.State            (StateT)
import Data.Binary                    (Binary)


-- -----------------------------
-- Types
-- -----------------------------

-- | defines back-end implementation
data HistoryImpl st = HistoryImpl {
      doInit              :: Agent st
    , castWriteResult       :: WriteResult -> HistoryAgent st ()
    , callFindResults       :: FindResults -> HistoryAgent st [Result]
    , doShutdown          :: ShutdownHandler st
}

type HistoryAgent st = StateT st Agent

data HistoryFail = HCallFailed CallFail
        deriving (Show, Eq, Typeable, Generic)
instance Binary HistoryFail
instance NFData HistoryFail where rnf = genericRnf

instance Convertible CallFail HistoryFail where
    safeConvert cf = return (HCallFailed cf)

data WriteResult = WriteResult Result
    deriving (Show, Typeable, Generic)

instance Binary WriteResult
instance NFData WriteResult where rnf = genericRnf

instance ServerCast WriteResult where
    type CastProtocol WriteResult = HistoryImpl
    castName _ = serverName
    handle = castWriteResult

data FindResults = AllResultsSince UTCTime
                 | ActionResultsSince Key UTCTime
    deriving (Show, Typeable, Generic)

instance Binary FindResults
instance NFData FindResults where rnf = genericRnf

instance ServerCall FindResults where
    type CallProtocol FindResults = HistoryImpl
    type CallResponse FindResults = [Result]
    callName _ = serverName
    respond = callFindResults

-- -----------------------------
-- API
-- -----------------------------

writeResult        :: (MonadAgent agent)
                   => Result -> agent (Either CallFail ())
writeResult result' =
    castServ $ WriteResult result'

findResultsSince :: (MonadAgent agent)
                   => UTCTime -> agent (Either HistoryFail [Result])
findResultsSince time = findHistory (AllResultsSince time)

findActionResultsSince :: (MonadAgent agent)
                       => Key -> UTCTime -> agent (Either HistoryFail [Result])
findActionResultsSince key' = findHistory . ActionResultsSince key'


serverName :: String
serverName = "agent:executive:history"

findHistory :: (MonadAgent agent)
            => FindResults -> agent (Either HistoryFail [Result])
findHistory command = do
    eresult <- callServ command
    case eresult of
        Right result' -> return $ Right result'
        Left cf -> return (Left $ HCallFailed cf)
