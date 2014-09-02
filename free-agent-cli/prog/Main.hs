{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import FreeAgent.AgentPrelude
import FreeAgent.Core
import FreeAgent.Cli.CmdArgs

main :: IO ()
main = configArgs def >>= print . configNodeHost . fst
