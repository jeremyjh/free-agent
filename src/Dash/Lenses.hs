{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- Tuck away all the lens classes and instances - also export
-- types for convenience - modules that needs lenses should only import lenses
module Dash.Lenses
( module Dash.Types
, module Dash.Lenses
)
where

import Dash.Prelude
import Dash.Types

makeFields ''AgentConfig
makeFields ''Wrapped
