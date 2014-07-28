{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module FreeAgent.Core.Action.ShellCommand
    ( ShellCommand(..)
    , ShellResult(..)
    , RegexMatch (..)
    , defaultShellCommand
    )

where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Core.Action ()
import           FreeAgent.Orphans ()

import           Data.Default (Default(..))
import           Shelly
    ( run, shelly, chdir, setenv, silently
    , errExit, lastExitCode, lastStderr)
import           Text.Regex (mkRegex, matchRegex)
import Control.Error (left, right )

type RegexString = String

data RegexMatch
  = MatchAll
  | AndMatch              RegexMatch RegexMatch
  | OrMatch               RegexMatch RegexMatch
  | MatchStdioSuccess     RegexString
  | MatchStdioFail        RegexString
  | MatchStderrSuccess    RegexString
  | MatchStderrFail       RegexString
  deriving (Show, Eq, Typeable, Generic)

instance Semigroup RegexMatch where
    (<>) = AndMatch

instance Monoid RegexMatch where
    mempty = MatchAll
    mappend = (<>)

instance Default RegexMatch where
    def = mempty

data ShellCommand
  = ShellCommand { shellCommand      :: FilePath
                 , shellChdir        :: FilePath
                 , shellArgs         :: [Text]
                 , shellEnv          :: [(Text, Text)]
                 , shellSuccessCodes :: [Int]
                 , shellFailCodes    :: [Int]
                 , shellRegexMatch   :: RegexMatch
                 } deriving (Show, Eq, Typeable, Generic)

instance Default ShellCommand where
    def = ShellCommand {
              shellCommand      = mempty
            , shellChdir        = "./"
            , shellArgs         = mempty
            , shellEnv          = mempty
            , shellSuccessCodes = mempty
            , shellFailCodes    = mempty
            , shellRegexMatch   = mempty
            }

defaultShellCommand :: ShellCommand
defaultShellCommand = def

data ShellResult
 = ShellResult { shellStdout     :: Text
               , shellStderr     :: Text
               , shellExitCode   :: Int
               , shellTimestamp  :: UTCTime
               , shellResultOf   :: ShellCommand
               } deriving (Show, Eq, Typeable, Generic)

instance Stashable ShellCommand where
    key = fpToText . shellCommand

instance Stashable ShellResult where
    key = key . shellResultOf

instance Extractable ShellResult

instance Resulting ShellResult where
    summary ShellResult{..} =
        ResultSummary shellTimestamp
                      shellStdout
                      (Action shellResultOf)

instance Extractable ShellCommand

instance Runnable ShellCommand ShellResult where
    exec cmd@ShellCommand{..} =
        shelly . errExit False
               . silently
               . chdir shellChdir $
         do forM_ shellEnv (uncurry setenv)
            runEitherT $
             do cmdStdout <- tryAnyConvT $ run shellCommand shellArgs
                cmdStderr <- lift lastStderr
                exitCode <- lift lastExitCode
                case (shellSuccessCodes, shellFailCodes) of
                    ([], []) ->
                        when (exitCode /= 0) $
                            left (UnknownResponse $ " Unknown exit code: " <> tshow exitCode)
                    ([], fails) ->
                        when (exitCode `elem` fails) $
                            left (GeneralFailure $ " Fails on exit code: " <> tshow exitCode)
                    (passes, _) ->
                        when (exitCode `notElem` passes) $
                            left (GeneralFailure $ " Fails on exit code: " <> tshow exitCode)
                if checkMatch (convert cmdStdout) (convert cmdStderr) shellRegexMatch
                then
                 do time <- getCurrentTime
                    right $ ShellResult cmdStdout cmdStderr exitCode time cmd
                else left (GeneralFailure $ "Match failed: " <> tshow shellRegexMatch
                                         <> "\n In output: " <> cmdStdout
                                         <> "\n" <> cmdStderr )
      where
        checkMatch sout serr matcher =
            case matcher of
                MatchAll -> True
                AndMatch one two ->
                    checkMatch sout serr one && checkMatch sout serr two
                OrMatch one two ->
                    checkMatch sout serr one || checkMatch sout serr two
                MatchStdioSuccess sregex ->
                    isJust (matchRegex (mkRegex sregex) sout)
                MatchStderrSuccess sregex ->
                    isJust (matchRegex (mkRegex sregex) serr)
                MatchStdioFail sregex ->
                    isNothing (matchRegex (mkRegex sregex) sout)
                MatchStderrFail sregex ->
                    isNothing (matchRegex (mkRegex sregex) serr)

deriveSerializers ''RegexMatch
deriveSerializers ''ShellCommand
deriveSerializers ''ShellResult
