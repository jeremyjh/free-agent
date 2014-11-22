{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies                                           #-}


module FreeAgent.Core.Protocol.Executive
    (
      ExecImpl(..)
    , ExecutiveCommand(..)
    , StoreAction(..)
    , QueryActions(..)
    , RemoveAction(..)
    , ExecuteStored(..)
    , ExecuteBatch(..)
    , AddListener(..)
    , executeStored
    , executeAction
    , matchAction
    , matchResult
    , serverName
    , ExecFail(..)
    )
where

import FreeAgent.AgentPrelude
import FreeAgent.Core.Action
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Process              as Process
import FreeAgent.Server.ManagedAgent


import Control.Error                  (hoistEither, (??))
import Control.Monad.State            (StateT)
import Data.Binary


-- -----------------------------
-- Types
-- -----------------------------

data ExecFail = ECallFailed CallFail
              | EDeserializationFailure Text
              | ActionNotFound Key
              | ActionFailed !Text
              | DBException !Text
              deriving (Show, Eq, Typeable, Generic)

instance Binary ExecFail

instance Convertible RunnableFail ExecFail where
    safeConvert = return . ActionFailed . tshow

instance Convertible CallFail ExecFail where
    safeConvert = return . ECallFailed

data ExecutiveCommand =
        ExecuteAction Action
      | TerminateExecutive
      deriving (Show, Typeable, Generic)

instance Binary ExecutiveCommand where
instance NFData ExecutiveCommand where

type ExecImplM st rs = StateT st Agent rs
type ExecImplE st rs = ExecImplM st (Either ExecFail rs)

data ExecImpl st = ExecImpl {
     callStoreAction   :: StoreAction -> ExecImplM st ()
   , callRemoveAction  :: RemoveAction -> ExecImplM st ()
   , callExecuteStored :: ExecuteStored -> ExecImplE st Result
   , callQueryActions  :: QueryActions -> ExecImplM st [Action]
   , castExecuteStored :: ExecuteStored -> ExecImplM st ()
   , castExecuteBatch  :: ExecuteBatch -> ExecImplM st ()
   , castAddListener   :: AddListener -> ExecImplM st ()
}


data StoreAction = StoreAction Action
                 | StoreNewerAction Action UTCTime
                 | StoreActions [Action]
      deriving (Show, Typeable, Generic)

instance Binary StoreAction

instance ServerCall StoreAction where
    type CallResponse StoreAction = ()
    type CallProtocol StoreAction = ExecImpl
    callName _ = serverName
    respond = callStoreAction

data RemoveAction = RemoveAction !Key
      deriving (Show, Typeable, Generic)

instance Binary RemoveAction

instance ServerCall RemoveAction where
    type CallProtocol RemoveAction = ExecImpl
    callName _ = serverName
    respond = callRemoveAction

data ExecuteStored = ExecuteStored !Key
    deriving (Show, Typeable, Generic)

instance Binary ExecuteStored

instance ServerCall ExecuteStored where
    type CallResponse ExecuteStored = Either ExecFail Result
    type CallProtocol ExecuteStored = ExecImpl
    callName _ = serverName
    respond = callExecuteStored


instance ServerCast ExecuteStored where
    type CastProtocol ExecuteStored = ExecImpl
    castName _ = serverName
    handle = castExecuteStored


data ExecuteBatch = ExecuteBatch ![Key]
    deriving (Show, Typeable, Generic)

instance Binary ExecuteBatch
instance NFData ExecuteBatch where rnf = genericRnf

instance ServerCast ExecuteBatch where
    type CastProtocol ExecuteBatch = ExecImpl
    castName _ = serverName
    handle = castExecuteBatch

data AddListener = AddListener (Closure Listener)
    deriving (Show, Typeable, Generic)

instance Binary AddListener
instance NFData AddListener

instance ServerCast AddListener where
    type CastProtocol AddListener = ExecImpl
    castName _ = serverName
    handle = castAddListener

data QueryActions = QueryActions
    deriving (Show, Eq, Typeable, Generic)

instance Binary QueryActions
instance NFData QueryActions

instance ServerCall QueryActions where
    type CallResponse QueryActions = [Action]
    type CallProtocol QueryActions = ExecImpl
    callName _ = serverName
    respond = callQueryActions

-- | Execute 'Action' corresponding to 'Key' with the current
-- target server and extract the concrete result (if possible) to
-- the expected type.
executeStored :: (MonadAgent agent, Resulting result)
                  => Key -> agent (Either ExecFail result)
executeStored key' =
    runEitherT $
     do result' <- callResult >>= convEitherT >>= hoistEither
        extract result' ?? EDeserializationFailure (tshow result')
  where callResult = lift $ callServ $ ExecuteStored key'

executeAction :: (MonadAgent agent, Runnable a b)
              => a -> agent (Either ExecFail Result)
executeAction action' =
    callExecutive $ ExecuteAction (toAction action')

-- | Used with 'addListener' - defines a 'Listener' that will
-- receive a 'Result' for each 'Action' executed that matches the typed predicate
-- argument. Only predicates for Actions in which the underlying concrete type
-- matches will be evaluated.
matchAction :: Runnable a b => (a -> Bool) -> NodeId -> String -> Listener
matchAction af = ActionMatching (matchA af)

-- | Used with 'addListener' - defines a 'Listener' that will
-- receive a 'Result' for each 'Action' executed where both the Action and Result
-- match the predicate arguments.
-- If you need the 'ActionMatcher' predicate to have access to the underlying
-- concrete type, then pass the typed predicate to 'matchA' to make an
-- 'ActionMatcher'.
matchResult :: Resulting b
            => ActionMatcher -> (b -> Bool) -> NodeId -> String -> Listener
matchResult af rf = ResultMatching af (matchR rf)

serverName :: String
serverName = "agent:executive"

callExecutive :: (NFSerializable a, MonadAgent agent)
            => ExecutiveCommand -> agent (Either ExecFail a)
callExecutive command = do
    eresult <- callTarget serverName command
    case eresult of
        Right result' -> return result'
        Left cf -> return (Left $ ECallFailed cf)

instance NFData ExecFail where rnf = genericRnf
instance NFData StoreAction where rnf = genericRnf
instance NFData RemoveAction where rnf = genericRnf
instance NFData ExecuteStored where rnf = genericRnf
