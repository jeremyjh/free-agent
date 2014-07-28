{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

-- Tuck away all the lens classes and instances - also export
-- types for convenience - modules that needs lenses should only import lenses
module FreeAgent.Core.Internal.Lenses
( module Control.Lens
, module FreeAgent.Core.Internal.Types
, module FreeAgent.Core.Internal.Lenses
)
where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Types

import Control.Lens
       ( makeFields, makeLenses, Getting, use, uses, view, views, (&)
       , (.~), (^.), _1, _2, set, to, _Right, (%=), (%~), (.=)
       , Profunctor, Lens', Optical)

import Control.Applicative(Const)



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

viewsConfig :: forall f p r a. (ContextReader f, Profunctor p)
            => (p a (Const r a) -> AgentConfig -> Const r AgentConfig) -> p a r -> f r
viewsConfig lens f = views (agentConfig.lens) f <$> askContext

viewsContext :: forall f p r a. (ContextReader f, Profunctor p)
             => Optical p (->) (Const r) AgentContext AgentContext a a -> p a r -> f r
viewsContext lens f = views lens f <$> askContext
