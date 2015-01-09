{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies                   #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                               #-}


module FreeAgent.Core.Server.Executive
    ( execServer
    ) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core
import           FreeAgent.Core.Lenses
import           FreeAgent.Database.AcidState
import           FreeAgent.Server.ManagedAgent
import           FreeAgent.Core.Protocol.Executive (serverName)


import           Control.Monad.Reader               (ask)
import Control.Monad.State (StateT)
import qualified Data.Map.Strict                    as Map

import           Control.Distributed.Static         (unclosure)
import           Control.Error                      ((!?))
import           Data.Default                       (Default (..))

-- -----------------------------
-- Types
-- -----------------------------
type RunningActions = Map Key ProcessId

type StoredAction = (Action, UTCTime)

data ExecPersist
  = ExecPersist { _persistActions   :: Map Key StoredAction
                , _persistListeners :: [Closure Listener]
                } deriving (Show, Typeable)

makeFields ''ExecPersist

instance Default ExecPersist where
    def = ExecPersist mempty []

type ExecAgent a = EitherT ExecFail (StateT ExecState Agent) a

data ExecState
  = ExecState { _stateRunning   :: !RunningActions
              , _stateListeners :: ![Listener]
              , _stateAcid      :: !(AcidState ExecPersist)
              , _stateCachedActions  :: !(Map Key (ExecAgent Result))
              , _stateExecutedCount  :: !Int
              } deriving (Typeable, Generic)

makeFields ''ExecState

-- -----------------------------
-- Persistent state functions
-- -----------------------------

putAction :: StoredAction -> Update ExecPersist ()
putAction stored@(action', _) =
    actions %= Map.insertWith newer (key action') stored
  where newer new@(_, ntime) old@(_,otime)
          | ntime > otime = new
          | otherwise = old

deleteAction :: Key -> Update ExecPersist ()
deleteAction key' =
    actions %= Map.delete key'

getAction :: Key -> Query ExecPersist (Maybe StoredAction)
getAction key' = views actions (Map.lookup key')

allActions :: Query ExecPersist [Action]
allActions = views actions (fmap (fst . snd) . Map.toList)

putListener :: Closure Listener -> Update ExecPersist ()
putListener listener = listeners %= (:) listener

getPersist :: Query ExecPersist ExecPersist
getPersist = ask

-- we have to make the splices near the top of the file
$(makeAcidic ''ExecPersist ['putAction, 'getAction, 'deleteAction,'allActions, 'putListener
                           ,'getPersist])


-- -----------------------------
-- Implementation
-- -----------------------------

execServer :: AgentServer
execServer =
    defineServer
        serverName
        initExec
        defaultProcess {
            apiHandlers =
            [
              registerCallAsync execImpl (Proxy :: Proxy ExecuteAction)
            , registerCallAsync execImpl (Proxy :: Proxy ExecuteStored)
            , registerCall execImpl (Proxy :: Proxy StoreAction)
            , registerCall execImpl (Proxy :: Proxy RemoveAction)
            , registerCall execImpl (Proxy :: Proxy QueryActions)
            , registerCast execImpl (Proxy :: Proxy ExecuteStored)
            , registerCast execImpl (Proxy :: Proxy ExecuteBatch)
            , registerCast execImpl (Proxy :: Proxy AddListener)
            ]
          , shutdownHandler = \(AgentState stats _ _) _ -> do
                pid <- getSelfPid
                say $ "Executive Stats: " ++ show stats
                say $ "Executive server " ++ show pid ++ " shutting down."
        }
  where initExec = do
            listeners' <- join $ viewPlugins listeners
            acid' <- openOrGetDb "agent-executive" def def
            persist <- query' acid' GetPersist
            rt <- viewConfig remoteTable
            let cls = rights $ map (unclosure rt) (persist ^. listeners)
            let caches = map (doExec . fst) (persist ^. actions)
            return $ ExecState (Map.fromList []) (listeners' ++ cls) acid' caches 0

type instance ProtoT rq ExecState a = StateT ExecState Agent a

execImpl :: ExecImpl ExecState
execImpl = ExecImpl callExecuteAction' callStoreAction' callRemoveAction'
                    callExecuteStored' callQueryActions'
                    castExecuteStored' castExecuteBatch' castAddListener'
  where
    callExecuteAction' cmd@(ExecuteAction action') =
        runLogEitherT cmd $ doExec action'

    callStoreAction' (StoreAction action')  =
        do now <- getCurrentTime
           void $ update (PutAction (action',now))
           cacheAction action'
    callStoreAction' (StoreNewerAction action' time) =
     do void $ update (PutAction (action', time))
        cacheAction action'
    callStoreAction' (StoreActions actions')  =
        do now <- getCurrentTime
           forM_ actions' $ \action' ->
            do void $ update (PutAction (action',now))
               cacheAction action'

    callRemoveAction' (RemoveAction key')  =
     do void $ update (DeleteAction key')
        cachedActions %= Map.delete key'

    callExecuteStored' cmd@(ExecuteStored key') =
     do executedCount %= (+) 1
        runLogEitherT cmd $
            join (uses cachedActions (lookup key') !? ActionNotFound key')

    callQueryActions' _ = query AllActions

    castExecuteStored' cmd =
     do executedCount %= (+) 1
        void . spawnLocal . void $ callExecuteStored' cmd

    castExecuteBatch'(ExecuteBatch keys') =
        forM_ keys' $ \key' ->
         do executedCount %= (+) 1
            void . spawnLocal . void $ callExecuteStored' (ExecuteStored key')

    castAddListener' (AddListener cl) =
      do rt <- viewConfig remoteTable
         case unclosure rt cl of
             Left msg -> [qwarn|AddListener failed! Could not evaluate
                           new listener closure: #{msg}|]
             Right listener -> do
                 listeners %= (:) listener
                 update (PutListener cl)

cacheAction :: Action -> StateT ExecState Agent ()
cacheAction action' = cachedActions %= Map.insert (key action') (doExec action')

doExec :: Action -> ExecAgent Result
doExec action' = do
    result <- tryExec action' >>= convEitherT
    storeResult result >>= notifyListeners
  where
    storeResult result = do
        [qdebug| Storing result: #{result}|]
        writeResult result >>= convEitherT
        return result
    notifyListeners result = do
        listeners' <- uses listeners (filter fst . map exMatch)
        [qdebug| Checking match for #{length listeners'} listeners |]
        forM_ listeners' $ \(_,addr) -> do
            [qdebug|Sending Result: #{result} To: #{addr}|]
            send addr result
        return result
      where
        exMatch (ActionMatching afilter nodeid name') =
            (afilter action', (nodeid,name'))
        exMatch (ResultMatching afilter rfilter nodeid name') =
            (afilter action' && rfilter result, (nodeid, name'))


deriveSafeStore ''ExecPersist
