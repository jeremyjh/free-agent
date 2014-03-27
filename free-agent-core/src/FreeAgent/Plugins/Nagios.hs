{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}


module FreeAgent.Plugins.Nagios
    ( Command(..)
    , CheckTCP(..)
    , CommandResult(..)
    , NagiosResult(..)
    , NagiosConfig(..)
    , pluginDef
    ) where

import           FreeAgent.Action
import           FreeAgent.Core
import           FreeAgent.Lenses
import           AgentPrelude

import           Data.Time.Clock   (getCurrentTime)
import           System.Exit       (ExitCode (..))
import           System.Process    (readProcessWithExitCode)

import           Control.Error (runEitherT, hoistEither)
import           Data.Default      (Default (..))
import qualified Data.Serialize as Cereal (encode)



-- | Plugin-specific configuration
data NagiosConfig = NagiosConfig {_nagiosPluginsPath :: FilePath}
                        deriving (Show, Eq, Typeable, Generic)

instance Default NagiosConfig where
    def = NagiosConfig "/usr/lib/nagios/plugins/"

makeFields ''NagiosConfig

-- | Supported Actions
data Command = Command { _commandHost         :: Text
                       , _commandPort         :: Maybe Int
                       , _commandShellCommand :: Text
                       } deriving (Show, Eq, Typeable, Generic)
makeFields ''Command
deriveSerializers ''Command

instance Stashable Command where
    key cmd = convert $ cmd^.host

data CommandResult = OK | Warning | Critical | Unknown
    deriving (Show, Eq, Typeable, Generic)
deriveSerializers ''CommandResult

-- | Provides the PluginDef for the Nagios plugin. Provide this to
-- 'addPlugin' in the 'registerPlugins' block in your app config/main.
-- Provide a NagiosConfig record - use 'def' for default values
--
-- > addPlugin $ Nagios.pluginDef def { _nagiosPluginPath = ... }
data CheckTCP = CheckTCP { _checktcpHost :: Text
                         , _checktcpPort :: Int
                         } deriving (Show, Eq, Typeable, Generic)
makeFields ''CheckTCP
deriveSerializers ''CheckTCP

data NagiosResult
  = NagiosResult {_nagresResultSummary :: ResultSummary
                 ,_nagresResult :: CommandResult
                 }
  deriving (Show, Eq, Typeable, Generic)
makeFields ''NagiosResult
deriveSerializers ''NagiosResult

pluginDef :: NagiosConfig -> PluginDef
pluginDef conf = definePlugin "Nagios" conf (return []) [] $
 do register (actionType :: Command)
    register (actionType :: CheckTCP)

extractConfig' :: (ContextReader m) => m NagiosConfig
extractConfig' = extractConfig $ pluginDef def ^.name

instance Stashable NagiosResult where
    key (NagiosResult (ResultSummary time _ _) _) = Cereal.encode time

instance Resulting NagiosResult where
    summary (NagiosResult s _) = s

instance Runnable Command NagiosResult where
    exec cmd =
        catchAny (
             do cmdPath <- commandPath
                (rc, result', _) <- liftIO $ readProcessWithExitCode cmdPath makeArgs []
                case rc of
                    ExitSuccess   -> completeAs OK result'
                    ExitFailure 1 -> completeAs Warning result'
                    ExitFailure 2 -> completeAs Critical result'
                    ExitFailure i -> return $
                        Left $ UnknownResponse $ tshow i ++ ": " ++ convert result' )
             (\ exception -> do
                putStrLn $ "Command exec threw exception: " ++ tshow exception
                return $ Left $ RIOException $ tshow exception )
      where
        makeArgs = ["-H", convert $ cmd^.host, "-p", portS $ cmd^.port]
        portS (Just p) = show p
        portS Nothing = ""
        completeAs cmdres result' = do
            time <- liftIO getCurrentTime
            let summ = ResultSummary time (convert result') (Action cmd)
            return $ Right $ NagiosResult summ cmdres
        commandPath = do
            nagconf <- extractConfig'
            let cmdPath = (nagconf^.pluginsPath) </> convert (cmd^.shellCommand)
            return $ fpToString cmdPath

instance Stashable CheckTCP where
    key c = convert $ c^.host ++ ":" ++ tshow (c^.port)

instance Runnable CheckTCP NagiosResult where
    exec cmd = runEitherT $ do
        result' <- exec (Command (cmd^.host) (Just $ cmd^.port) "check_tcp")
                   >>= hoistEither
        return $ result' & resultSummary.resultOf .~ Action cmd
