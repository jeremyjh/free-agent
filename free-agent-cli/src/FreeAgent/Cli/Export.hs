{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Cli.Export
    (exportActions) where

import FreeAgent.AgentPrelude
import FreeAgent.Core                (Agent, key)
import FreeAgent.Server.Executive    (QueryActions(..))
import FreeAgent.Server.ManagedAgent (callServ)

import Control.Error                 (left)
import Data.EitherR                  (runEitherRT, succeedT)
import Data.Yaml                     (encodeFile)
import Shelly                        (shelly, test_d, test_f)

data Mode = File | Dir

selectMode :: (MonadIO m) => FilePath -> m (Either Text Mode)
selectMode fp = shelly . runEitherT . runEitherRT $
  do isFile <- lift $ test_f fp
     when isFile (succeedT File)
     isDir <- lift $ test_d fp
     when isDir (succeedT Dir)
     succeedT File -- doesn't exist, make a new file

exportActions :: FilePath -> Agent (Either Text ())
exportActions fp =
  runEitherT $
    do mode <- selectMode fp
       case mode of
           Right File -> exportFile fp
           Right Dir -> exportDirectory fp
           Left reason -> left reason
       return ()

exportFile :: FilePath -> EitherT Text Agent ()
exportFile fp =
  do actions <- callServ QueryActions >>= convEitherT
     case actions of
        []       -> return ()
        actions' -> liftIO $ encodeFile (convert fp) actions'

exportDirectory :: FilePath -> EitherT Text Agent ()
exportDirectory fp =
  do actions <- callServ QueryActions >>= convEitherT
     case actions of
        []       -> return ()
        actions' -> liftIO $
            forM_ actions' $ \ action ->
                let file = fp </> convert (key action ++ ".yaml")
                in encodeFile (convert file) action
