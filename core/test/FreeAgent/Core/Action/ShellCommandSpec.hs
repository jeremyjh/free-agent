{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FreeAgent.Core.Action.ShellCommandSpec (main, spec) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core
import           FreeAgent.Core.Lenses
import           FreeAgent.Core.Action.ShellCommand
import           FreeAgent.Core.Action.Composition

import           FreeAgent.TestHelper

import qualified Data.Binary as Binary

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
                Right (Just res) <- fmap extractResult <$> exec cmd
                return (shellStdout res)
            `shouldReturn` "val1\n"

        it "can serialize and deserialize existentially" $ do
           testAgent $ do
               let cmd = (defaultShellCommand "nostdout")
                          {   shellCommand = "bash"
                          ,   shellArgs = ["-c", "echo hello"]
                          ,   shellRegexMatch = MatchStdioFail "^hello$"
                          }
               let action' = toAction cmd
                   bytes = Binary.encode action'
               Right decoded <- decodeComposite $ Binary.decode bytes
               print decoded
               return True
           `shouldReturn` True

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
                Right (Just res) <- fmap extractResult <$> exec cmd
                return (shellStdout res)
            `shouldReturn` "/usr/bin\n"

testAgent ma = quickRunAgent 500
                             ("4122"
                             , appConfig & nodePort .~ "4122"
                             , appPlugins
                             ) ma
