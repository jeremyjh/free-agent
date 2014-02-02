{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

-- Tuck away all the lens classes and instances - also export
-- types for convenience - modules that needs lenses should only import lenses
module FreeAgent.Lenses
( module Control.Lens
, module FreeAgent.Types
, module FreeAgent.Lenses
)
where

import           AgentPrelude
import           FreeAgent.Types

import           Control.Lens      (Getting, makeFields, use, view, (&), (.~),
                                    (^.), _1, _2, set, to)

makeFields ''AgentContext
makeFields ''AgentConfig
makeFields ''Wrapped
makeFields ''PluginDef
makeFields ''ResultSummary
makeFields ''Package
makeFields ''AgentServer

-- | Use a lens to view a portion of AgentContext
viewConfig :: (ContextReader m) => Getting a AgentContext a -> m a
viewConfig lens = view lens <$> askContext
