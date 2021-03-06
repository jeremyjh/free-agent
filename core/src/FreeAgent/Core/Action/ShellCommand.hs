{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts              #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell              #-}

module FreeAgent.Core.Action.ShellCommand
    ( ShellCommand(..)
    , ShellResult(..)
    , RegexMatch (..)
    , defaultShellCommand
    )

where

import Control.Monad.Trans.Except     (throwE)
import FreeAgent.AgentPrelude
import FreeAgent.Core.Action
import FreeAgent.Core.Internal.Lenses
import FreeAgent.Orphans              ()

import Data.Aeson.TH                  (Options (..), defaultOptions, deriveJSON)
import Data.Binary                    (Binary)
import Data.Char                      as Char (toLower)
import Data.Default                   (Default (..))
import Shelly                         (chdir, errExit, lastExitCode, lastStderr, run,
                                       setenv, shelly, silently)
import Text.Regex                     (matchRegex, mkRegex)

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
  = ShellCommand { shellKey          :: Key
                 , shellCommand      :: FilePath
                 , shellChdir        :: FilePath
                 , shellArgs         :: [Text]
                 , shellEnv          :: [(Text, Text)]
                 , shellSuccessCodes :: [Int]
                 , shellFailCodes    :: [Int]
                 , shellRegexMatch   :: RegexMatch
                 } deriving (Show, Eq, Typeable, Generic)

instance Default ShellCommand where
    def = ShellCommand {
              shellKey          = mempty
            , shellCommand      = mempty
            , shellChdir        = "./"
            , shellArgs         = mempty
            , shellEnv          = mempty
            , shellSuccessCodes = mempty
            , shellFailCodes    = mempty
            , shellRegexMatch   = mempty
            }

defaultShellCommand :: Key -> ShellCommand
defaultShellCommand k = def {shellKey = k}

data ShellResult
 = ShellResult { shellStdout    :: Text
               , shellStderr    :: Text
               , shellExitCode  :: Int
               , shellTimestamp :: UTCTime
               , shellResultOf  :: ShellCommand
               } deriving (Show, Eq, Typeable, Generic)

instance Stashable ShellCommand where
    key = shellKey

instance Runnable ShellCommand where
    exec cmd@ShellCommand{..} =
        shelly . errExit False
               . silently
               . chdir shellChdir $
         do forM_ shellEnv (uncurry setenv)
            runExceptT $
             do cmdStdout <- tryAnyConvT $ run shellCommand shellArgs
                cmdStderr <- lift lastStderr
                exitCode <- lift lastExitCode
                case (shellSuccessCodes, shellFailCodes) of
                    ([], []) ->
                        when (exitCode /= 0) $
                            throwE (UnknownResponse $ " Unknown exit code: " <> tshow exitCode)
                    ([], fails) ->
                        when (exitCode `elem` fails) $
                            throwE (GeneralFailure $ " Fails on exit code: " <> tshow exitCode)
                    (passes, _) ->
                        when (exitCode `notElem` passes) $
                           throwE (GeneralFailure $ " Fails on exit code: " <> tshow exitCode)
                if checkMatch (convert cmdStdout) (convert cmdStderr) shellRegexMatch
                then
                 do time <- getCurrentTime
                    let result = ShellResult cmdStdout cmdStderr exitCode time cmd
                    return (Result (wrap result) time cmdStdout (toAction cmd))
                else throwE (GeneralFailure $ "Match failed: " <> tshow shellRegexMatch
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
deriveSerializers ''ShellResult

-- we want to customize the JSON field names for ShellCommand
-- so it looks nicer in Yaml, which may be very frequently used
instance Binary ShellCommand
instance NFData ShellCommand where rnf = genericRnf
deriveSafeStore ''ShellCommand

deriveJSON (defaultOptions {fieldLabelModifier = \field ->
                                let (x:xs) = drop 5 field
                                in Char.toLower x : xs
                           })
           ''ShellCommand
