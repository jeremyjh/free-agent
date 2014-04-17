{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module FreeAgent.Action.Composition where

import           AgentPrelude
import           FreeAgent.Lenses
import           FreeAgent.Action
import           FreeAgent.Process

{-import Data.SafeCopy(SafeCopy)-}
{-import Data.Binary (Binary)-}


data ActionPlan
  = Exec Action
  | Sequential ActionPlan ActionPlan
  | Parallel ActionPlan ActionPlan
  | OnFailure ActionPlan ActionPlan
  | OnSuccess ActionPlan ActionPlan
  deriving (Show, Eq, Typeable, Generic)

instance Stashable ActionPlan where
    key = firstKey
      where firstKey (Exec action') = key action'
            firstKey _ = error "TODO: finish this instance"

data ResultList
  = ResultList { listSummary :: ResultSummary
               , _listResults :: [Result]
               }
    deriving ( Show, Eq, Typeable, Generic)

makeFields ''ResultList

instance Stashable ResultList where
    key = key . summary

instance Resulting ResultList where
    summary = listSummary

planExec :: Actionable a b => a -> ActionPlan
planExec = Exec . toAction

thenExec :: Actionable a b => ActionPlan -> a -> ActionPlan
thenExec = combinePlan Sequential

whileExec :: Actionable a b => ActionPlan -> a -> ActionPlan
whileExec = combinePlan Parallel

onSuccess :: Actionable a b => ActionPlan -> a -> ActionPlan
onSuccess = combinePlan OnSuccess

onFailure :: Actionable a b => ActionPlan -> a -> ActionPlan
onFailure = combinePlan OnFailure

combinePlan :: Actionable a b
             => (ActionPlan -> ActionPlan -> ActionPlan)
             -> ActionPlan -> a -> ActionPlan
combinePlan fn plan action' = fn plan (planExec action')

example :: ActionPlan
example =
    planExec NoOp
    `thenExec` NoOp
    <> planExec NoOp
    `whileExec` NoOp

exampleExec :: Agent ()
exampleExec = undefined

instance Semigroup ActionPlan where
    (<>) = Sequential

instance Semigroup ResultList where
    list1 <> list2 =
        list1 & results %~ (<>) (list2^.results)

instance Runnable ActionPlan ResultList where
    exec (Exec action') = do
        eresult <- exec action'
        return $ case eresult of
            Right result' ->
                let (ResultSummary rtime rtext _) = summary result'
                in Right (ResultList (ResultSummary rtime rtext action') [result'])
            Left fails -> Left fails

    exec (Sequential plan1 plan2) = do
        result1 <- exec plan1
        result2 <- exec plan2
        return $ fmap (<>) result1 <*> result2

    exec (Parallel plan1 plan2) = do
        ref1 <- async (exec plan1)
        ref2 <- async (exec plan2)
        aresult1 <- wait ref1
        aresult2 <- wait ref2
        return $ case aresult1 of
            AsyncDone result1 ->
                case aresult2 of
                    AsyncDone result2 -> result1 <> result2
                    reason -> Left (GeneralFailure $ "Async operation failed: " ++ (tshow reason ))
            reason -> Left (GeneralFailure $ "Async operation failed: " ++ (tshow reason ))

    exec (OnSuccess _ _) = undefined
    exec (OnFailure _ _) = undefined


-- NoOp type is just temporarily here for examples
data NoOp = NoOp deriving (Show, Eq, Typeable, Generic)

instance Stashable NoOp where key = undefined
instance Runnable NoOp Result where exec = undefined
deriveSerializers ''NoOp

deriveSerializers ''ResultList
deriveSerializers ''ActionPlan
