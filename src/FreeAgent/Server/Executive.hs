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


module FreeAgent.Server.Executive
    ( execServer
    , registerAction
    , unregisterAction
    , executeRegistered
    , executeAction
    , addListener
    , matchAction
    , matchResult
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

import           Control.Error                                       hiding (err)
import           Control.Monad.State                                 (StateT)
import           Control.Distributed.Static                          (unclosure)
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
registerAction :: (MonadAgent m, Actionable a b) => Target -> a -> m (Either Text ())
registerAction target action' = do
    callExecutive target $ RegisterAction (toAction action')

unregisterAction :: (MonadAgent m) => Target -> Key -> m (Either Text ())
unregisterAction target key' = callExecutive target $ UnregisterAction key'

executeRegistered :: (MonadAgent m) => Target -> Key -> m EResult
executeRegistered target key' = do
    callExecutive target $ ExecuteRegistered key'

executeAction :: (MonadAgent m, Actionable a b) => Target -> a -> m EResult
executeAction target action' = do
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
            [ handleRpcChan $ agentAsyncCallHandlerE $
                  \cmd -> case cmd of
                      ExecuteAction act -> doExecuteAction act
                      ExecuteRegistered k -> doExecuteRegistered k
                      _ -> $(err "illegal pattern match")

            , handleRpcChan $ agentAsyncCallHandlerE $ \cmd -> perform cmd

            , handleCast $ agentCastHandler $ \ (AddListener cl) -> do
                rt <- viewConfig remoteTable
                case unclosure rt cl of
                    Left msg -> [qwarn|AddListener failed! Could not evaluate
                                     new listener closure: #{msg}|]
                    Right listener -> listeners %= (:) listener
            ]

        } where
            perform (RegisterAction a)  = doRegisterAction a
            perform (UnregisterAction k) = doUnRegisterAction k
            perform _ = $(err "illegal pattern match")

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
            => Target -> ExecutiveCommand -> m (Either Text a)
callExecutive Local command = syncCallChan execServer command
callExecutive (Remote peer) command = syncCallChan (peer, execServer) command
callExecutive (Route contexts' zones') command = runEitherT $ do
    peer <- resolveRoute contexts' zones'
    syncCallChan (peer, execServer) command >>= hoistEither

resolveRoute :: (MonadAgent m) => [Context] -> [Zone] -> EitherT Text m Peer
resolveRoute contexts' zones' = do
    peers <- lift $ Peer.queryPeerServers execServer
                                          (Set.fromList contexts')
                                          (Set.fromList zones')
    foundPeer peers
  where foundPeer peers
            | peers == Set.empty = do
                [qwarn| Could not resolve a Route
                        with Context: #{contexts'} and Zone: #{zones'}|]
                left "Could not find a suitable Peer."
            | otherwise = let peer:_ = Set.toList peers in right peer

type ExecAgent a = EitherT Text (StateT ExecState Agent) a

-- ExecutiveCommand realizations
doRegisterAction ::  Action -> ExecAgent ()
doRegisterAction act = do
    [qdebug|Registering action: #{act}|]
    void . agentDb $ stashAction act


doUnRegisterAction :: Key -> ExecAgent ()
doUnRegisterAction k = do
    [qdebug|Unregistering action key: #{k}|]
    agentDb $ deleteAction k

doExecuteAction :: Action -> ExecAgent Result
doExecuteAction act = do
    [qdebug|Executing action: #{act}|]
    doExec act

doExecuteRegistered :: Key -> ExecAgent Result
doExecuteRegistered k = do
    [qdebug|Executing action key: #{k}|]
    act <- agentDb $ fetchAction k
    case act of
        Right a -> do
            [qdebug|Executing action: #{a}|]
            doExec a
        Left (ParseFail msg) -> do
            [qwarn| Unable to retrieve action key: #{k} - #{msg}|]
            left $ toT msg
        Left (NotFound _) -> do
            [qwarn| Unable to retrieve action key: #{k}
                            not found in database. |]
            left "Action not found in database."

instance ContextReader m => ContextReader (EitherT e m) where
    askContext = lift askContext

doExec :: Action -> ExecAgent Result
doExec act =
    exec act >>= processResult >>= hoistEither
  where
    processResult (Left msg) = do
            [qwarn| Execution of action: #{key act} has failed
            with message: #{msg} |]
            return $ Left msg
    processResult (Right r) = Right <$> (storeResult r >>= notifyListeners)
    storeResult result = do
        [qdebug| Storing result: #{result}|]
        asyncAgentDb $ withKeySpace (KS.actionsAp $ key act) $
            store (timestampKey result) result
        return result
    notifyListeners result = do
        listeners' <- use listeners
        [qdebug| Checking match for #{length listeners'} listeners |]
        forM_ listeners' $ \listener ->
            let (matched,pid) = case listener of
                   ActionMatching afilter apid ->
                            (afilter act, apid)
                   ResultMatching afilter rfilter apid ->
                            (afilter act && rfilter result, apid) in
            when matched $ do
                [qdebug|Sending Result: #{result} To: #{pid}|]
                send pid result
        return result
    timestampKey = toBytes . view timestamp . summary
