{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module FreeAgent.Database.KeySpace where

import AgentPrelude
import FreeAgent.Types (KeySpace)

type KeySpacer = KeySpace -> KeySpace

append :: KeySpace -> KeySpace -> KeySpace
append x y
    | length y > 0 = x ++ ":" ++ y
    | otherwise = x

agentAp :: KeySpacer
agentAp = append "agent"

actions :: KeySpace
actions = "actions"

actionsAp :: KeySpacer
actionsAp = append $ agentAp actions

packages :: KeySpace
packages = "packages"

packagesAp :: KeySpacer
packagesAp = append $ agentAp packages
