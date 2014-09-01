{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TemplateHaskell           #-}

-- Tuck away all the lens classes and instances - also export
-- types for convenience - modules that needs lenses should only import lenses
module FreeAgent.Core.Lenses
( module Control.Lens
, module FreeAgent.Core.Lenses
) where

import           FreeAgent.Core.Internal.Types
import           Control.Lens (makeFields, (&), (.~), (^.), _1, _2, to, (%=), (%~), (.=))


makeFields ''AgentConfig
makeFields ''PluginDef
