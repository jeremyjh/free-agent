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
{-# LANGUAGE TypeFamilies #-}


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
    , query, update
    )
where

import           AgentPrelude
import           FreeAgent.Action
import           FreeAgent.Core                                      (withAgent)
import           FreeAgent.Database
import           FreeAgent.Database.AcidState
import qualified FreeAgent.Database.KeySpace                         as KS
import           FreeAgent.Lenses
import           FreeAgent.Process                                   as Process
import           FreeAgent.Process.ManagedAgent
import           qualified FreeAgent.Server.Peer                               as Peer

import           Control.DeepSeq.TH                                  (deriveNFData)
import           Control.Monad.State                                 (StateT)
import Control.Monad.Reader (ask)
import           Data.Binary
import qualified Data.Map.Strict                                     as Map
import qualified Data.Set                                            as Set

import           Control.Distributed.Static                          (unclosure)
import           Control.Error                                       hiding (err)
import           Data.Default                          (Default(..))
import Data.Acid.Advanced (query')

-- -----------------------------
-- Types
-- -----------------------------
type RunningActions = Map Key ProcessId

data ExecPersist
  = ExecPersist { _persistActions :: Map ByteString Action
                , _persistListeners :: [Closure Listener]
                } deriving (Show, Typeable)

deriveSafeStore ''ExecPersist
makeFields ''ExecPersist

instance Default ExecPersist where
    def = ExecPersist mempty []

data ExecState
  = ExecState { _stateRunning   :: !RunningActions
              , _stateListeners :: ![Listener]
              , _stateAcid      :: !(AcidState ExecPersist)
              } deriving (Typeable, Generic)
makeFields ''ExecState

data ExecFail = RoutingFailed
              | ActionNotFound Key
              | ActionFailed !Text
              | DBException !Text
              deriving (Show, Eq, Typeable, Generic)

instance Binary ExecFail
deriveNFData ''ExecFail

instance Convertible RunnableFail ExecFail where
    convert = ActionFailed . tshow

data ExecutiveCommand =
        RegisterAction Action
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
-- Persistent state functions
-- -----------------------------

putAction :: Action -> Update ExecPersist ()
putAction action' =
    actions %= Map.insert (key action') action'

deleteAction :: Key -> Update ExecPersist ()
deleteAction key' =
    actions %= Map.delete key'

getAction :: Key -> Query ExecPersist (Maybe Action)
getAction key' = views actions (Map.lookup key')

putListener :: Closure Listener -> Update ExecPersist ()
putListener listener = listeners %= (:) listener

getPersist :: Query ExecPersist ExecPersist
getPersist = ask

-- we have to make the splices near the top of the file
$(makeAcidic ''ExecPersist ['putAction, 'getAction, 'deleteAction, 'putListener
                           ,'getPersist])

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
        Just acid' <- withAgent ctxt $ openOrGetDb "agent-executive" def def
        persist <- query' acid' GetPersist
        let cls = rights $ map (unclosure (ctxt^.remoteTable)) (persist^.listeners)
        let state' = ExecState (Map.fromList []) (listeners' ++ cls) acid'
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
                    Right listener -> do
                        listeners %= (:) listener
                        update (PutListener cl)
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
    update (PutAction action')
doRegistration (UnregisterAction key') =
    update (DeleteAction key')
doRegistration _ = $(err "illegal pattern match")

doExecuteAction :: Action -> ExecAgent Result
doExecuteAction = doExec

doExecuteRegistered :: Key -> ExecAgent Result
doExecuteRegistered key' = do
    action' <-  query (GetAction key') !? ActionNotFound key'
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
        listeners' <- uses listeners (filter fst . map exMatch)
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
