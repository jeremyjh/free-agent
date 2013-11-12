{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- Tuck away all the lens classes and instances - also export
-- types for convenience - modules that needs lenses should only import lenses
module FreeAgent.Lenses
( module FreeAgent.Types
, module FreeAgent.Lenses
)
where

import FreeAgent.Prelude
import FreeAgent.Types

makeFields ''AgentContext
makeFields ''Wrapped
