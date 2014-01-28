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


module FreeAgent.Executive where

import           FreeAgent.Action
import           FreeAgent.Core                                      (withAgent)
import           FreeAgent.Database
import qualified FreeAgent.Database.KeySpace                         as KS
import           FreeAgent.Lenses
import           FreeAgent.Process.ManagedAgent
import           AgentPrelude

import           Control.Monad.State                                 (StateT)
import           Data.Binary
import qualified Data.Map.Strict                                     as Map
import qualified Data.Set                                            as Set
import           Data.Time.Clock                                     (getCurrentTime)

import           FreeAgent.Process                  as Process
import           Control.Distributed.Process.Platform.Supervisor
import           Control.Distributed.Process.Platform.Time
       (Delay(..), milliSeconds)


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
                      | DeliverPackage Package
                      | RemovePackage UUID
                      {-| QueryActionHistory (ScanQuery ActionHistory [ActionHistory])-}
                      | TerminateExecutive
                      deriving (Typeable, Generic)
instance Binary ExecutiveCommand where
instance NFData ExecutiveCommand where

-- -----------------------------
-- API
-- -----------------------------
registerAction :: (MonadAgent m) => Action -> m ()
registerAction = callServer execServer . RegisterAction

executeRegistered :: (MonadAgent m) => Key -> m EResult
executeRegistered = callServer execServer . ExecuteRegistered

executeAction :: (MonadAgent m) => Action -> m EResult
executeAction = callServer execServer . ExecuteAction

-- | Send / register a package and optional list of Actions to register
-- to each context leader defined in the package
deliverPackage :: (MonadAgent m) => Package -> m [EResult]
deliverPackage p = callServer execServer $ DeliverPackage p

-- | Remove a package definition from the context leader(s). Does
-- not unregister Actions or remove history.
removePackage :: (MonadAgent m) => UUID -> m ()
removePackage = callServer execServer . RemovePackage

-- -----------------------------
-- Implementation
-- -----------------------------


execServer :: AgentServer
execServer = AgentServer "agent:executive" init child
  where
    init ctxt = do
        listeners' <- withAgent ctxt $ join $ viewConfig listeners
        let state' = ExecState (Map.fromList []) listeners'
        serve state' (initState ctxt) executiveProcess
      where
        executiveProcess = defaultProcess {
            apiHandlers =
            [ handleRpcChan $ agentAsyncCallHandler $
                  \cmd -> case cmd of
                      ExecuteAction act -> doExecuteAction act
                      ExecuteRegistered k -> doExecuteRegistered k
                      _ -> $(err "illegal pattern match")

            , handleRpcChan $ agentAsyncCallHandler $
                \(DeliverPackage pkg) -> doDeliverPackage pkg

            , handleRpcChan $ agentAsyncCallHandler $ \cmd -> perform cmd
            ]

        } where
            perform (RegisterAction a)  = doRegisterAction a
            perform (UnregisterAction k) = doUnRegisterAction k
            perform (RemovePackage u) = doRemovePackage u
            perform _ = $(err "illegal pattern match")

    child ctxt = do
        initChild <- toChildStart $ init ctxt

        return $ ChildSpec {
              childKey     = ""
            , childType    = Worker
            , childRestart = Permanent
            , childStop    = TerminateTimeout (Delay $ milliSeconds 10)
            , childStart   = initChild
            , childRegName = Just $ LocalName "agent:executive"
        }

type ExecAgent a = StateT ExecState Agent a

instance (ContextReader m)
      => ContextReader (StateT ExecState m) where
    askContext = lift askContext

-- ExecutiveCommand realizations
doRegisterAction ::  Action -> ExecAgent ()
doRegisterAction act = do
    [qdebug|Registering action: #{act}|]
    void . agentDb $ stashAction act

doUnRegisterAction :: Key -> ExecAgent ()
doUnRegisterAction k = do
    [qdebug|Unregistering action key: #{k}|]
    agentDb $ deleteAction k

doExecuteAction :: Action -> ExecAgent EResult
doExecuteAction act = do
    [qdebug|Executing action: #{act}|]
    doExec act

doExecuteRegistered :: Key -> ExecAgent EResult
doExecuteRegistered k = do
    [qdebug|Executing action key: #{k}|]
    act <- agentDb $ fetchAction k
    case act of
        Right a -> do
            [qdebug|Executing action: #{a}|]
            doExec a
        Left (ParseFail msg) -> do
            [qwarn| Unable to retrieve action key: #{k} - #{msg}|]
            undefined
        Left (NotFound _) -> do
            [qwarn| Unable to retrieve action key: #{k}
                            not found in database. |]
            undefined

doExec :: Action -> ExecAgent EResult
doExec act =
    exec act >>= processResult
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

-- | Register and schedule a list of Actions
doDeliverPackage :: Package -> ExecAgent [EResult]
doDeliverPackage package = do
    [qdebug|Delivering package: #{package}|]
    doLocal <- (&&) <$> localContext <*> localZone
    if doLocal then registerIt >>= storeIt >>= scheduleIt
               else return [] --TODO: implement routing
  where
    localContext :: ExecAgent Bool
    localContext = do
        supported <- viewConfig (agentConfig.contexts)
        let found = Set.intersection (package^.contexts) supported
        return $ not $ Set.null found
    localZone :: ExecAgent Bool
    localZone = do
        supported <- viewConfig (agentConfig.zones)
        let found = Set.intersection (package^.zones) supported
        return $ not $ Set.null found
    registerIt = do
        keys <- forM (package^.actions) $ \act -> do
            doRegisterAction act
            return $ key act
        return $ package & actionKeys .~ (keys ++ (package^.actionKeys))
                         & actions .~ []
    storeIt = agentDb . withKeySpace KS.packages . stash
    scheduleIt pkg =
        case pkg^.schedule of
            Now -> do
                -- execute all actions
                -- TODO: execute concurrently w/ Platform.Asnc
                results <- forM (pkg^.actionKeys) $ \akey ->
                    doExecuteRegistered akey

                -- record package execution history
                time <- liftIO getCurrentTime
                agentDb . withKeySpace (KS.packagesAp $ key pkg) $
                    store (toBytes time) pkg

                return results

            _ -> return [] --TODO: add to scheduler

doRemovePackage :: UUID -> ExecAgent ()
doRemovePackage uid = do
    [qdebug|Unregistering package: #{uid}|]
    agentDb . withKeySpace KS.packages . delete $ toBytes uid
