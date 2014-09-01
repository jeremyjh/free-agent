{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE TemplateHaskell           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Tuck away all the lens classes and instances - also export
-- types for convenience - modules that needs lenses should only import lenses
module FreeAgent.Core.Internal.Lenses
( module FreeAgent.Core.Internal.Lenses
, module X
)
where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Lenses as X
import           FreeAgent.Core.Internal.Types as X

import Control.Lens as X
       ( makeLenses, Getting, use, uses, view, views, set
       , Profunctor, Lens', Optical)

import Control.Applicative(Const)



makeFields ''AgentContext
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
