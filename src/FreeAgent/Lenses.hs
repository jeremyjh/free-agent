{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- Tuck away all the lens classes and instances - also export
-- types for convenience - modules that needs lenses should only import lenses
module FreeAgent.Lenses
( module Control.Lens
, module FreeAgent.Types
, module FreeAgent.Lenses
)
where

import FreeAgent.Prelude
import FreeAgent.Types
import           Control.Lens
    (makeFields, (.~), (^.), (&), view, Getting, _1, _2, use)

makeFields ''AgentContext
makeFields ''AgentConfig
makeFields ''Wrapped
makeFields ''PluginDef
makeFields ''ResultSummary

-- | Use a lens to view a portion of AgentContext
viewConfig :: (ConfigReader m) => Getting a AgentContext a -> m a
viewConfig lens = do
    conf <- askConfig
    return $ view lens conf
