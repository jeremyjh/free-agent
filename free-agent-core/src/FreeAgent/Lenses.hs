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

import Control.Lens
       ( makeFields, makeLenses, Getting, use, uses, view, views, (&)
       , (.~), (^.), _1, _2, set, to, _Right, (%=), (%~), (.=)
       , Profunctor, Lens')

import Control.Lens.Type (Overloading)
import Control.Lens.Getter (Accessor)


makeFields ''AgentContext
makeFields ''AgentConfig
makeFields ''PluginDef
makeFields ''ResultSummary
makeFields ''Peer
makeFields ''AgentServer
makeFields ''PluginSet

-- | Use a lens to view a portion of AgentContext
viewConfig :: (ContextReader m) => Getting a AgentConfig a -> m a
viewConfig lens = view (agentConfig.lens) <$> askContext

-- | Use a lens to view a portion of AgentContext
viewContext :: (ContextReader m) => Getting a AgentContext a -> m a
viewContext lens = view lens <$> askContext

viewsConfig :: (Profunctor p, ContextReader f)
            => Overloading p (->) (Accessor r) AgentConfig AgentConfig a a
            -> p a r -> f r
viewsConfig lens f = views (agentConfig.lens) f <$> askContext

viewsContext :: (Profunctor p, ContextReader f) => Overloading p (->) (Accessor r) AgentContext AgentContext a a -> p a r -> f r
viewsContext lens f = views lens f <$> askContext
