{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE CPP, TypeFamilies                                           #-}


module FreeAgent.Core.Protocol.Executive
    (
      ExecImpl(..)
    , ExecuteAction(..)
    , StoreAction(..)
    , QueryActions(..)
    , RemoveAction(..)
    , ExecuteStored(..)
    , ExecuteBatch(..)
    , AddListener(..)
    , executeStored
    , executeAction
    , actionListener
    , resultListener
    , serverName
    , ExecFail(..)
    )
where

import FreeAgent.AgentPrelude
import FreeAgent.Core.Action
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Core.Protocol
import FreeAgent.Process              as Process


import Control.Error                  (hoistEither)
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


data ExecImpl st = ExecImpl {
     callExecuteAction :: ExecuteAction -> ProtoT ExecuteAction st (Either ExecFail Result)
   , callStoreAction   :: StoreAction -> ProtoT StoreAction st ()
   , callRemoveAction  :: RemoveAction -> ProtoT RemoveAction st ()
   , callExecuteStored :: ExecuteStored -> ProtoT ExecuteStored st (Either ExecFail Result)
   , callQueryActions  :: QueryActions -> ProtoT QueryActions st [Action]
   , castExecuteStored :: ExecuteStored -> ProtoT ExecuteStored st ()
   , castExecuteBatch  :: ExecuteBatch -> ProtoT ExecuteBatch st ()
   , castAddListener   :: AddListener -> ProtoT AddListener st ()
}

#define EXEC_CALL(REQ, RSP, FN)             \
instance ServerCall REQ where               \
   type CallProtocol REQ = ExecImpl        ;\
   type CallResponse REQ = RSP             ;\
   callName _ = serverName                 ;\
   respond = FN                            ;\

data ExecuteAction = ExecuteAction Action
      deriving (Show, Typeable, Generic)

EXEC_CALL(ExecuteAction, Either ExecFail Result, callExecuteAction)
instance Binary ExecuteAction
instance NFData ExecuteAction where rnf = genericRnf

data StoreAction = StoreAction Action
                 | StoreNewerAction Action UTCTime
                 | StoreActions [Action]
      deriving (Show, Typeable, Generic)
instance Binary StoreAction
instance NFData StoreAction where rnf = genericRnf
EXEC_CALL (StoreAction, (), callStoreAction)

data RemoveAction = RemoveAction !Key
      deriving (Show, Typeable, Generic)

instance Binary RemoveAction
EXEC_CALL (RemoveAction, (), callRemoveAction)

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
executeStored :: (MonadAgent agent)
                  => Key -> agent (Either ExecFail Result)
executeStored key' =
    runExceptT $ callResult >>= convExceptT >>= hoistEither
  where callResult = lift $ callServ $ ExecuteStored key'

executeAction :: (MonadAgent agent, Runnable a)
              => a -> agent (Either ExecFail Result)
executeAction action' =
    callExecutive $ ExecuteAction (toAction action')

-- | Used with 'addListener' - defines a 'Listener' that will
-- receive a 'Result' for each 'Action' executed that matches the typed predicate
-- argument. Only predicates for Actions in which the underlying concrete type
-- matches will be evaluated.
actionListener :: Runnable a => (a -> Bool) -> NodeId -> String -> Listener
actionListener af = ActionMatching (matchAction af)

-- | Used with 'addListener' - defines a 'Listener' that will
-- receive a 'Result' for each 'Action' executed where both the Action and Result
-- match the predicate arguments.
-- If you need the 'ActionMatcher' predicate to have access to the underlying
-- concrete type, then pass the typed predicate to 'matchA' to make an
-- 'ActionMatcher'.
resultListener :: Portable b
               => ActionMatcher -> (b -> Bool)
               -> NodeId -> String -> Listener
resultListener af rf = ResultMatching af (matchResult rf)

serverName :: String
serverName = "agent:executive"

callExecutive :: (NFSerializable a, MonadAgent agent)
            => ExecuteAction -> agent (Either ExecFail a)
callExecutive command = do
    eresult <- callTarget serverName command
    case eresult of
        Right result' -> return result'
        Left cf -> return (Left $ ECallFailed cf)

instance NFData ExecFail where rnf = genericRnf
instance NFData RemoveAction where rnf = genericRnf
instance NFData ExecuteStored where rnf = genericRnf
