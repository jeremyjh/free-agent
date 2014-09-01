{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import FreeAgent
import FreeAgent.AgentPrelude
import FreeAgent.Cli.CmdArgs

main :: IO ()
main = configArgs def >>= print . configNodeHost . fst
