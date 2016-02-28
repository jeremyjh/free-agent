{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, TemplateHaskell          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Tuck away all the lens classes and instances - also export
-- types for convenience - modules that needs lenses should only import lenses
module FreeAgent.Core.Internal.Lenses
( module FreeAgent.Core.Internal.Lenses
, module X
)
where

import FreeAgent.AgentPrelude
import FreeAgent.Core.Internal.Types as X
import FreeAgent.Core.Lenses         as X

import Control.Lens                  as X (Getting, Lens', LensLike',
                                           makeLenses)

import Control.Applicative           (Const)



makeFields ''AgentContext
makeFields ''AgentServer
makeFields ''PluginSet


-- | Use a lens to view config from AgentContext
viewConfig :: (ContextReader m) => Getting a AgentConfig a -> m a
viewConfig lens = view (agentConfig.lens) <$> askContext

-- | Use a lens to view plugins from AgentContext
viewPlugins :: (ContextReader m) => Getting a PluginSet a -> m a
viewPlugins lens = view (plugins.lens) <$> askContext

-- | Internal use only - view access to entire context
viewContext :: (ContextReader m) => Getting a AgentContext a -> m a
viewContext lens = view lens <$> askContext

viewsConfig :: ContextReader f => ((a -> Const b a) -> AgentConfig -> Const b AgentConfig) -> (a -> b) -> f b
viewsConfig lens f = views (agentConfig.lens) f <$> askContext

viewsContext :: ContextReader f => LensLike' (Const b) AgentContext a -> (a -> b) -> f b
viewsContext lens f = views lens f <$> askContext
