{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module FreeAgent.Core.Action.Composition where

import           AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Core.Action
import           FreeAgent.Process


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

data ResultList
  = ResultList ResultSummary [Result]
    deriving (Show, Eq, Typeable, Generic)

instance Stashable ResultList where
    key = key . summary

instance Resulting ResultList where
    summary (ResultList summ _) = summ

instance Semigroup ResultList where
    (ResultList summ results1) <> (ResultList _ results2) =
        ResultList summ (results1 <> results2)

planExec :: Runnable a b => a -> ActionPlan
planExec = Exec . toAction

thenExec :: Runnable a b => ActionPlan -> a -> ActionPlan
thenExec = combinePlan Sequential

whileExec :: Runnable a b => ActionPlan -> a -> ActionPlan
whileExec = combinePlan Parallel

onFailure :: Runnable a b => ActionPlan -> a -> ActionPlan
onFailure = combinePlan OnFailure

combinePlan :: Runnable action result
             => (ActionPlan -> ActionPlan -> ActionPlan)
             -> ActionPlan -> action -> ActionPlan
combinePlan fn plan action' = fn plan (planExec action')


failResult :: RunnableFail -> ResultSummary -> Result
failResult reason summary' = Result $ FailResult reason summary'

instance Runnable ActionPlan ResultList where
    exec (Exec action') =
        runEitherT $ do
            result' <- tryExecET action'
            let (ResultSummary rtime rtext _) = summary result'
            return $ ResultList (ResultSummary rtime rtext action') [result']

    exec (Sequential plan1 plan2) =
        runEitherT $ do
            result1 <- tryExecET plan1
            result2 <- tryExecWithET plan2 (Result result1)
            return $ result1 <> result2

    exec (Parallel plan1 plan2) = do
        ref1 <- async (tryExec plan1)
        ref2 <- async (tryExec plan2)
        aresult1 <- wait ref1
        aresult2 <- wait ref2
        return $ case aresult1 of
            AsyncDone (Right result1) ->
                case aresult2 of
                    AsyncDone (Right result2) -> Right $ result1 <> result2
                    reason -> Left (GeneralFailure $ "Async exec failed: " ++ tshow reason )
            reason -> Left (GeneralFailure $ "Async exec failed: " ++ tshow reason )

    exec (OnFailure plan1 plan2) = do
        result' <- tryExec plan1
        case result' of
            Left reason -> do
                summary' <- resultNow (tshow reason) plan1
                tryExecWith plan2 (failResult reason summary')
            _ -> return result'

    execWith (Exec action') eresult =
        runEitherT $ do
            result' <- tryExecWithET action' eresult
            let (ResultSummary rtime rtext _) = summary result'
            return $ ResultList (ResultSummary rtime rtext action') [result']

    execWith (Sequential plan1 plan2) eresult =
        runEitherT $ do
            result1 <- tryExecWithET plan1 eresult
            result2 <- tryExecWithET plan2 (Result result1)
            return (result1 <> result2)

    execWith (Parallel plan1 plan2) eresult = do
        ref1 <- async (tryExecWith plan1 eresult)
        ref2 <- async (tryExecWith plan2 eresult)
        aresult1 <- wait ref1
        aresult2 <- wait ref2
        return $ case aresult1 of
            AsyncDone (Right result1) ->
                case aresult2 of
                    AsyncDone (Right result2) -> Right $ result1 <> result2
                    reason -> Left (GeneralFailure $ "Async operation failed: " ++ tshow reason)
            reason -> Left (GeneralFailure $ "Async operation failed: " ++ tshow reason)

    execWith (OnFailure plan1 plan2) result = do
        result1 <- tryExecWith plan1 result
        case result1 of
            Left reason -> do
                summary' <- resultNow (tshow reason) plan1
                execWith plan2 (failResult reason summary')
            _ -> return result1

deriveSerializers ''ResultList
deriveSerializers ''ActionPlan
