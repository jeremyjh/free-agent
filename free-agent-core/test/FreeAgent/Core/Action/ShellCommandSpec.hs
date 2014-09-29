{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Core.Action.ShellCommandSpec (main, spec) where

import           FreeAgent.AgentPrelude
import qualified Prelude as P
import           FreeAgent.Core
import           FreeAgent.Core.Lenses
import           FreeAgent.Core.Action.ShellCommand

import           FreeAgent.TestHelper
import           FreeAgent.Fixtures

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "ShellCommand - as used by CheckTCP" $ do
        it "can set environment variables, pass args and match stdout" $ do
            testAgent $ do
                let (env1, val1) = ("env1", "val1")
                let cmd = (defaultShellCommand "setenv")
                          {   shellCommand = "bash"
                          ,   shellArgs = ["-c", "echo $env1"]
                          ,   shellEnv = [(env1, val1)]
                          ,   shellRegexMatch = MatchStdioSuccess "^va.1$"
                          }
                Right res <- exec cmd
                return $ take 36 (show res)
            `shouldReturn` "ShellResult {shellStdout = \"val1\\n\","

        it "can NOT match stdout" $ do
            testAgent $ do
                let cmd = (defaultShellCommand "nostdout")
                          {   shellCommand = "bash"
                          ,   shellArgs = ["-c", "echo hello"]
                          ,   shellRegexMatch = MatchStdioFail "^hello$"
                          }
                Left (GeneralFailure msg) <- exec cmd
                return msg
            `shouldReturn` "Match failed: MatchStdioFail \"^hello$\"\n In output: hello\n\n"

        it "can change dir temporarily" $ do
            testAgent $ do
                let cmd = (defaultShellCommand "chdir")
                          {   shellCommand = "pwd"
                          ,   shellChdir = "/usr/bin"
                          ,   shellRegexMatch = MatchStdioSuccess "/usr/bin"
                          }
                Right res <- exec cmd
                return $ take 36 (show res)
            `shouldReturn` "ShellResult {shellStdout = \"/usr/bin"

testAgent ma = quickRunAgent 500
                             ("4122"
                             , appConfig & nodePort .~ "4122"
                             , appPlugins
                             ) ma
