{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}


module FreeAgent.Server.Executive
    ( execServer
    , registerAction
    , unregisterAction
    , executeRegistered
    , executeAction
    , addListener
    , matchAction
    , matchResult
    , ExecFail(..)
    )
where

import           FreeAgent.Action
import           FreeAgent.Core                                      (withAgent)
import           FreeAgent.Database
import qualified FreeAgent.Database.KeySpace                         as KS
import           FreeAgent.Lenses
import qualified FreeAgent.Server.Peer                               as Peer
import           FreeAgent.Process.ManagedAgent
import           AgentPrelude

import           Control.DeepSeq.TH                                  (deriveNFData)
import           Control.Distributed.Static                          (unclosure)
import           Control.Error                                       hiding (err)
import           Control.Monad.State                                 (StateT)
import           Data.Binary
import qualified Data.Map.Strict                                     as Map
import qualified Data.Set                                            as Set

import           FreeAgent.Process                  as Process

-- -----------------------------
-- Types
-- -----------------------------
type RunningActions = Map Key ProcessId

data ExecState
  = ExecState { _stateRunning   :: !RunningActions
              , _stateListeners :: ![Listener]
              } deriving (Typeable, Generic)
makeFields ''ExecState

data ExecFail = RoutingFailed
              | EFetchFailed !FetchFail
              | ActionFailed !Text
              | DBException !Text
              deriving (Show, Eq, Typeable, Generic)
instance Binary ExecFail
deriveNFData ''ExecFail

instance Convertible FetchFail ExecFail where
    convert = EFetchFailed

instance Convertible Text ExecFail where
    convert = ActionFailed

-- -----------------------------
-- Types
-- -----------------------------

data ExecutiveCommand = RegisterAction Action
                      | UnregisterAction Key
		              | ExecuteAction Action
		              | ExecuteRegistered Key
                      {-| QueryActionHistory (ScanQuery ActionHistory [ActionHistory])-}
                      | TerminateExecutive
                      | AddListener (Closure Listener)
                      deriving (Show, Typeable, Generic)
instance Binary ExecutiveCommand where
instance NFData ExecutiveCommand where

-- -----------------------------
-- API
-- -----------------------------
registerAction :: (MonadAgent m, Actionable a b)
               => Target -> a -> m (Either ExecFail ())
registerAction target action' =
    callExecutive target $ RegisterAction (toAction action')

unregisterAction :: (MonadAgent m) => Target -> Key -> m (Either ExecFail ())
unregisterAction target key' =
    callExecutive target $ UnregisterAction key'

executeRegistered :: (MonadAgent m)
                  => Target -> Key -> m (Either ExecFail Result)
executeRegistered target key' =
    callExecutive target $ ExecuteRegistered key'

executeAction :: (MonadAgent m, Actionable a b)
              => Target -> a -> m (Either ExecFail Result)
executeAction target action' =
    callExecutive target $ ExecuteAction (toAction action')

-- | Asynchronously subscribe to a local or remote Executive service
-- Throws an exception if a Route is supplied which cannot be resolved
addListener :: (MonadAgent m)
            => Target -> Closure Listener -> m ()
addListener Local cl = cast execServer (AddListener cl)
addListener (Remote peer) cl = cast (peer, execServer) (AddListener cl)
addListener (Route contexts' zones') cl = do
    peer <- forceEitherT $ resolveRoute contexts' zones'
    cast (peer,execServer) (AddListener cl)

-- | Used with 'addListener' - defines a 'Listener' that will
-- receive a 'Result' for each 'Action' executed that matches the typed predicate
-- argument. Only predicates for Actions in which the underlying concrete type
-- matches will be evaluated.
matchAction :: (Runnable a b) => (a -> Bool) -> ProcessId -> Listener
matchAction af = ActionMatching (matchA af)

-- | Used with 'addListener' - defines a 'Listener' that will
-- receive a 'Result' for each 'Action' executed where both the Action and Result
-- match the predicate arguments.
-- If you need the 'ActionMatcher' predicate to have access to the underlying
-- concrete type, then pass the typed predicate to 'matchA' to make an
-- 'ActionMatcher'.
matchResult :: (Resulting b)
            => ActionMatcher -> (b -> Bool) -> ProcessId -> Listener
matchResult af rf = ResultMatching af (matchR rf)

-- -----------------------------
-- Implementation
-- -----------------------------

execServer :: AgentServer
execServer = AgentServer sname init child
  where
    sname = "agent:executive"
    init ctxt = do
        listeners' <- withAgent ctxt $ join $ viewConfig listeners
        let state' = ExecState (Map.fromList []) listeners'
        serve state' initExec executiveProcess
      where
        initExec state' = do
            pid <- getSelfPid
            Peer.registerServer execServer pid
            return $ InitOk (AgentState ctxt state') Infinity

        executiveProcess = defaultProcess {
            apiHandlers =
            [ handleRpcChan $ agentAsyncCallHandlerET $
                  \cmd -> case cmd of
                      ExecuteAction act -> doExecuteAction act
                      ExecuteRegistered k -> doExecuteRegistered k
                      _ -> $(err "illegal pattern match")

            , handleRpcChan $ agentAsyncCallHandlerET $ \cmd -> doRegistration cmd

            , handleCast $ agentCastHandler $ \ (AddListener cl) -> do
                rt <- viewConfig remoteTable
                case unclosure rt cl of
                    Left msg -> [qwarn|AddListener failed! Could not evaluate
                                     new listener closure: #{msg}|]
                    Right listener -> listeners %= (:) listener
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
            , childRegName = Just $ LocalName sname
        }

callExecutive :: (NFSerializable a, MonadAgent m)
            => Target -> ExecutiveCommand -> m (Either ExecFail a)
callExecutive Local command = syncCallChan execServer command
callExecutive (Remote peer) command = syncCallChan (peer, execServer) command
callExecutive (Route contexts' zones') command = runEitherT $ do
    peer <- resolveRoute contexts' zones'
    syncCallChan (peer, execServer) command >>= hoistEither

resolveRoute :: (MonadAgent m) => [Context] -> [Zone] -> EitherT ExecFail m Peer
resolveRoute contexts' zones' = do
    peers <- lift $ Peer.queryPeerServers execServer
                                          (Set.fromList contexts')
                                          (Set.fromList zones')
    foundPeer peers
  where foundPeer peers
            | peers == Set.empty = do
                [qwarn| Could not resolve a Route
                        with Context: #{contexts'} and Zone: #{zones'}|]
                left RoutingFailed
            | otherwise = let peer:_ = Set.toList peers in right peer

type ExecAgent a = EitherT ExecFail (StateT ExecState Agent) a

-- -----------------------------
-- Executive command realizations
-- -----------------------------

doRegistration :: ExecutiveCommand -> ExecAgent ()
doRegistration (RegisterAction action')  =
    void . agentDb $ stashAction action'
doRegistration (UnregisterAction key') =
    agentDb $ deleteAction key'
doRegistration _ = $(err "illegal pattern match")

doExecuteAction :: Action -> ExecAgent Result
doExecuteAction = doExec

doExecuteRegistered :: Key -> ExecAgent Result
doExecuteRegistered k = do
    action' <- agentDb (fetchAction k) >>= convEitherT
    doExec action'

doExec :: Action -> ExecAgent Result
doExec action' = do
    result <- exec action' >>= convEitherT
    storeResult result >>= notifyListeners
  where
    storeResult result = do
        [qdebug| Storing result: #{result}|]
        asyncAgentDb $ withKeySpace (KS.actionsAp $ key action') $
            store (timestampKey result) result
        return result
    notifyListeners result = do
        listeners' <- uses listeners ((filter fst) . (map exMatch))
        [qdebug| Checking match for #{length listeners'} listeners |]
        forM_ listeners' $ \(_,pid) -> do
            [qdebug|Sending Result: #{result} To: #{pid}|]
            send pid result
        return result
      where
        exMatch (ActionMatching afilter pid) =
            (afilter action', pid)
        exMatch (ResultMatching afilter rfilter pid) =
            (afilter action' && rfilter result, pid)
    timestampKey = toBytes . view timestamp . summary
