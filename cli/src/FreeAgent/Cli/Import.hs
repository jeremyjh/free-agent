{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Cli.Import
    (importActions, Mode(..), ImportItem(..)) where

import FreeAgent.AgentPrelude
import FreeAgent.Core                (Action, Agent)
import FreeAgent.Server.Executive    (StoreAction (..))
import FreeAgent.Server.Schedule (Event(..), ScheduleAddEvent(..))
import FreeAgent.Server.ManagedAgent (callServ)

import Control.Error (left)
import Data.EitherR                  (runEitherRT, succeedT)
import Data.Aeson
import Data.Yaml                     (decodeFileEither)
import Shelly                        (shelly, test_d, test_f, ls, hasExt)

data Mode = File | Dir

data ImportItem = ActionImport Action | EventImport Event
    deriving (Show)

instance FromJSON ImportItem where
    parseJSON (Object value') =
     do action' <- value' .:? "action"
        case action' of
            Just a -> return $ ActionImport a
            Nothing ->
             do event <- value' .: "event"
                return $ EventImport event
    parseJSON _ = mzero

instance ToJSON ImportItem where
    toJSON (ActionImport action') =
        object ["action" .= action']
    toJSON (EventImport event) =
        object ["event" .= event]

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
    imports <- join <$> mapM loadImportFile yamls
    importItems imports

importFile :: FilePath -> EitherT Text Agent ()
importFile fp =
 do imports <- loadImportFile fp
    importItems imports

importItems :: [ImportItem] -> EitherT Text Agent ()
importItems imports =
 do let (actions, events) = foldr appendImp ([],[]) imports
    unless (null actions) $
        callServ (StoreActions actions) >>= convEitherT
    unless (null events) $
        callServ (ScheduleAddEvents events) >>= convEitherT
  where
    appendImp (ActionImport action') (actions,events) = (action' : actions, events)
    appendImp (EventImport event) (actions,events) = (actions, event : events)

loadImportFile :: FilePath -> EitherT Text Agent [ImportItem]
loadImportFile fp =
 do esingle <- liftIO (decodeFileEither (convert fp))
    case esingle of
        Right single -> return [single]
        Left _ -> liftIO (decodeFileEither (convert fp)) >>= convEitherT
