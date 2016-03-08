{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                                           #-}


module FreeAgent.Core.Action.Composition where

import FreeAgent.AgentPrelude
import FreeAgent.Core.Action
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Process

import Control.Monad.Reader                     (runReaderT)

import Control.Distributed.Process.Async        (async, task)
import Control.Distributed.Process.Serializable (Serializable)

-- | Run an Agent action with an extracted AgentContext.
-- This is used to embed Agent code in the Process monad, for
-- example in ManagedProcess.
withAgent :: AgentContext -> Agent a -> Process a
withAgent ctxt ma =
    catchAny (runReaderT (unAgent ma) ctxt)
             $ \exception -> do
                 putStrLn $ "Exception in withAgent: " ++ tshow exception
                 throwIO exception

-- | Use d.p's 'async' function with an Agent computation.
agentAsync :: (MonadAgent agent, Serializable a)
           => Agent a -> agent (Async a)
agentAsync ma =
 do ctxt <- askContext
    liftP $ async $ task (withAgent ctxt ma)

-- | A composite 'Action', can be constructed with combinators
-- such as 'planExec' and 'thenExec'. The comments for each
-- Constructor type describe the exec strategy for the Runnable instance.
data ActionPlan
  -- ^ exec this Action - a leaf node.
  = Exec Action
  -- ^ Executes each plan sequentially, passing ResultList of first plan to execWith second plan
  -- if the first plan succeeds and returning a concatenated ResultList if
  -- both plans Succeed.
  | Sequential ActionPlan ActionPlan
  -- ^ Execute both plans in parallel using D.P.P Async. Returns
  -- concatenated ResultList if both plans succeed.
  | Parallel ActionPlan ActionPlan
  -- ^ Execute the second plan only if the first fails, wrapping the
  -- RunnableFail in a FailResult and passing this to execWith. Returns
  -- result of first plan if it succeeds, otherwise the second.
  | OnFailure ActionPlan ActionPlan
  deriving (Show, Eq, Typeable, Generic)

instance Semigroup ActionPlan where
    (<>) = Sequential

instance Stashable ActionPlan where
    --TODO: this is crazy
    key (Exec action') = "ActionPlan:" <> key action'
    key (Sequential plan _) = key plan
    key (Parallel plan _) = key plan
    key (OnFailure plan _) = key plan


decodeComposite :: ContextReader m => Action -> m (Either String Action)
decodeComposite action' =
    do Right decoded <- decodeEnvelope action'
       case extractAction decoded of
           Just plan ->  fmap (fmap toAction) (decodePlan plan)
           Nothing -> return $ Right decoded
   where decodePlan :: ContextReader m => ActionPlan -> m (Either String ActionPlan)
         decodePlan (Exec act) =
           do Right decode1 <- decodeComposite act
              return $ Right $ Exec decode1
         decodePlan (Sequential plan1 plan2) =
           do Right decode1 <- decodePlan plan1
              Right decode2 <- decodePlan plan2
              return $ Right $ Sequential decode1 decode2
         decodePlan (Parallel plan1 plan2) =
           do Right decode1 <- decodePlan plan1
              Right decode2 <- decodePlan plan2
              return $ Right $ Parallel decode1 decode2
         decodePlan (OnFailure plan1 plan2) =
           do Right decode1 <- decodePlan plan1
              Right decode2 <- decodePlan plan2
              return $ Right $ OnFailure decode1 decode2

planExec :: Runnable a => a -> ActionPlan
planExec = Exec . toAction

thenExec :: Runnable a => ActionPlan -> a -> ActionPlan
thenExec = combinePlan Sequential

whileExec :: Runnable a => ActionPlan -> a -> ActionPlan
whileExec = combinePlan Parallel

onFailure :: Runnable a => ActionPlan -> a -> ActionPlan
onFailure = combinePlan OnFailure

combinePlan :: Runnable action
             => (ActionPlan -> ActionPlan -> ActionPlan)
             -> ActionPlan -> action -> ActionPlan
combinePlan fn plan action' = fn plan (planExec action')


failResultNow :: MonadIO io => RunnableFail -> Text -> Action -> io Result
failResultNow reason summ action' =
    resultNow (FailResult reason (key action')) summ action'

data ResultList = ResultList Key [Result]
    deriving (Show, Eq, Typeable, Generic)

instance Stashable ResultList where
    key (ResultList key' _) = key'

appendResults :: Result -> Result -> Result
appendResults r1 r2 = r1 { resultWrapped = wrap $
    let list1 = extractResult r1
        list2 = extractResult r2 in
    case (list1, list2) of
      (Just (ResultList k results1 ), Just (ResultList _ results2)) ->
          ResultList k (results1 ++ results2)
      (Just (ResultList k results), Nothing) ->
          ResultList k (results ++ [r2])
      (Nothing, Just (ResultList k results)) ->
          ResultList k (r1 : results)
      (Nothing, Nothing) -> ResultList (key r1) [r1, r2]
    }

instance Runnable ActionPlan where
    type RunnableResult ActionPlan = ResultList

    exec (Exec action') = tryExec action'

    exec (Sequential plan1 plan2) =
        runExceptT $ do
            result1 <- tryExecET plan1
            result2 <- tryExecWithET plan2 result1
            return $ appendResults result1 result2

    exec (Parallel plan1 plan2) =
     do ref1 <- agentAsync (tryExec plan1)
        ref2 <- agentAsync (tryExec plan2)
        aresult1 <- wait ref1
        aresult2 <- wait ref2
        return $ case aresult1 of
            AsyncDone (Right result1) ->
                case aresult2 of
                    AsyncDone (Right result2) -> Right $ appendResults result1 result2
                    reason -> Left (GeneralFailure $ "Async exec failed: " ++ tshow reason)
            reason -> Left (GeneralFailure $ "Async exec failed: " ++ tshow reason)

    exec (OnFailure plan1 plan2) = do
        result' <- tryExec plan1
        case result' of
            Left reason -> do
                failed <- failResultNow reason (tshow reason) (toAction plan1)
                tryExecWith plan2 failed
            _ -> return result'

    execWith (Exec action') eresult = tryExecWith action' eresult

    execWith (Sequential plan1 plan2) eresult =
        runExceptT $ do
            result1 <- tryExecWithET plan1 eresult
            result2 <- tryExecWithET plan2 result1
            return $ appendResults result1 result2

    execWith (Parallel plan1 plan2) eresult =
     do ref1 <- agentAsync (tryExecWith plan1 eresult)
        ref2 <- agentAsync (tryExecWith plan2 eresult)
        aresult1 <- wait ref1
        aresult2 <- wait ref2
        return $ case aresult1 of
            AsyncDone (Right result1) ->
                case aresult2 of
                    AsyncDone (Right result2) -> Right $ appendResults result1 result2
                    reason -> Left (GeneralFailure $ "Async operation failed: " ++ tshow reason)
            reason -> Left (GeneralFailure $ "Async operation failed: " ++ tshow reason)

    execWith (OnFailure plan1 plan2) result = do
        result1 <- tryExecWith plan1 result
        case result1 of
            Left reason -> do
                failed <- failResultNow reason (tshow reason) (toAction plan1)
                tryExecWith plan2 failed
            _ -> return result1

deriveSerializers ''ActionPlan
deriveSerializers ''ResultList
