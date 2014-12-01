{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell                                                  #-}

-- Tuck away all the lens classes and instances - also export
-- types for convenience - modules that needs lenses should only import lenses
module FreeAgent.Core.Lenses
( module Control.Lens
, module FreeAgent.Core.Lenses
) where

import Control.Lens                  (abbreviatedFields, makeLensesWith, set, to, use,
                                      uses, view, views, (%=), (%~), (&), (.=), (.~),
                                      (^.), _1, _2)
import FreeAgent.Core.Internal.Types
import Language.Haskell.TH.Syntax    (Dec, Name, Q)

makeFields :: Name -> Q [Dec]
makeFields = makeLensesWith abbreviatedFields

makeLensesWith abbreviatedFields ''AgentConfig
makeLensesWith abbreviatedFields ''PluginDef
makeLensesWith abbreviatedFields ''ResultSummary
makeLensesWith abbreviatedFields ''Peer
