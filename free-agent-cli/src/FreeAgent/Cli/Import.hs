{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Cli.Import
    (importActions, exportActions) where

import FreeAgent.AgentPrelude
import FreeAgent.Core                (Action, Agent)
import FreeAgent.Server.Executive    (StoreAction (..))
import FreeAgent.Server.ManagedAgent (callServ)

import Control.Error                 (left)
import Data.EitherR                  (runEitherRT, succeedT)
import Data.Yaml                     (decodeFileEither)
import Shelly                        (shelly, test_d, test_f, ls, hasExt)
import System.Directory              (getModificationTime)

data Mode = File | Dir

importActions :: FilePath -> Agent (Either Text ())
importActions fp = runEitherT $
 do mode <- detectMode fp
    case mode of
        Right File -> importFile fp
        Right Dir -> importDirectory fp
        Left reason -> left reason
    return ()

detectMode :: (MonadIO m) => FilePath -> m (Either Text Mode)
detectMode fp = shelly . runEitherT . runEitherRT $
 do isFile <- lift $ test_f fp
    when isFile (succeedT File)
    isDir <- lift $ test_d fp
    when isDir (succeedT Dir)
    return "Import file or directory does not exist."

importDirectory :: FilePath -> EitherT Text Agent ()
importDirectory fp =
    do files <- shelly $ ls fp
       let yamls = filter (hasExt "yaml") files
       actions <- mapM loadActionFile yamls
       callServ (StoreActions actions) >>= convEitherT

importFile :: FilePath -> EitherT Text Agent ()
importFile fp =
 do action <- loadActionFile fp
    time <- liftIO $ getModificationTime (convert fp)
    callServ (StoreNewerAction action time) >>= convEitherT


loadActionFile :: FilePath -> EitherT Text Agent Action
loadActionFile fp =
    liftIO (decodeFileEither (convert fp)) >>= convEitherT

exportActions :: FilePath -> Agent (Either Text ())
exportActions = error "not done yet"
