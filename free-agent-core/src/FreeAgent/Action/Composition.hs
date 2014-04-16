{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

-- remove after noop is gone
{-# LANGUAGE MultiParamTypeClasses #-}

module FreeAgent.Action.Composition where

import           AgentPrelude
import           FreeAgent.Lenses
import           FreeAgent.Action


data Plan a
  = Exec a
  | DoNothing
  | Sequential (Plan a) (Plan a)
  | Parallel (Plan a) (Plan a)
  | OnFailure (Plan a) (Plan a)
  | OnSuccess (Plan a) (Plan a)
  deriving (Show, Eq, Typeable, Generic)

type ActionPlan = Plan Action

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

instance Monoid (Plan a) where
    mempty = DoNothing
    mappend = Sequential

instance Semigroup (Plan a) where
    (<>) = mappend

instance Functor Plan where
    fmap _ DoNothing = DoNothing
    fmap fn (Exec action') = Exec (fn action')
    fmap fn (Sequential plan1 plan2) = fmap fn plan1 `Sequential` fmap fn plan2
    fmap fn (Parallel plan1 plan2) = fmap fn plan1 `Parallel` fmap fn plan2
    fmap fn (OnSuccess plan1 plan2) = fmap fn plan1 `OnSuccess` fmap fn plan2
    fmap fn (OnFailure plan1 plan2) = fmap fn plan1 `OnFailure` fmap fn plan2

-- NoOp type is just temporarily here for examples
data NoOp = NoOp deriving (Show, Eq, Typeable, Generic)

instance Stashable NoOp where key = undefined
instance Runnable NoOp Result where exec = undefined

{-deriveSerializers ''Plan-}
deriveSerializers ''NoOp
