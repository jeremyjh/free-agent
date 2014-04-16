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

import           Control.Monad.State

data ActionPlan
  = Exec Action
  | DoNothing
  | Sequential ActionPlan ActionPlan
  | Parallel ActionPlan ActionPlan
  | OnFailure ActionPlan ActionPlan
  | OnSuccess ActionPlan ActionPlan
  deriving (Show, Eq, Typeable, Generic)

newtype Planning a
  = Planning {unPlanning :: State ActionPlan a}
        deriving (Functor, Applicative, Monad, MonadState ActionPlan)

toExec :: Actionable a b => a -> ActionPlan
toExec = Exec . toAction

runPlan :: Planning () -> ActionPlan
runPlan = snd . flip runState DoNothing . unPlanning

beginPlan :: Actionable a b => a -> Planning ()
beginPlan = put . toExec

thenExec :: Actionable a b => a -> Planning ()
thenExec action' = do
    pre <- get
    put $ Sequential pre (toExec action')

example :: ActionPlan
example = runPlan $ do
    beginPlan NoOp
    thenExec NoOp

-- NoOp type is just temporarily here for examples
data NoOp = NoOp deriving (Show, Eq, Typeable, Generic)

instance Stashable NoOp where key = undefined
instance Runnable NoOp Result where exec = undefined

deriveSerializers ''ActionPlan
deriveSerializers ''NoOp
