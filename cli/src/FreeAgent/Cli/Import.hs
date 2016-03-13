{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Cli.Import
    (importActions, Mode(..), ImportItem(..)) where

import FreeAgent.AgentPrelude
import FreeAgent.Core (Action, Agent)
import FreeAgent.Core.Action.Composition (decodeComposite)
import FreeAgent.Core.Protocol (callServ)
import FreeAgent.Core.Protocol.Executive (StoreAction (..))
import FreeAgent.Core.Protocol.Schedule (Event(..), ScheduleAddEvent(..))

import Control.Error (throwE)
import Data.EitherR (runExceptRT, succeedT)
import Data.Aeson
import Data.Yaml (decodeFileEither)
import Shelly (shelly, test_d, test_f, ls, hasExt)

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
importActions fp = runExceptT $
 do mode <- detectMode fp
    case mode of
        Right File -> importFile fp
        Right Dir -> importDirectory fp
        Left reason -> throwE reason
    return ()

detectMode :: (MonadIO m) => FilePath -> m (Either Text Mode)
detectMode fp = shelly . runExceptT . runExceptRT $
 do isFile <- lift $ test_f fp
    when isFile (succeedT File)
    isDir <- lift $ test_d fp
    when isDir (succeedT Dir)
    return "Import file or directory does not exist."

importDirectory :: FilePath -> ExceptT Text Agent ()
importDirectory fp =
 do files <- shelly $ ls fp
    let yamls = filter (hasExt "yaml") files
    imports <- join <$> mapM loadImportFile yamls
    importItems imports

importFile :: FilePath -> ExceptT Text Agent ()
importFile fp =
 do imports <- loadImportFile fp
    print imports
    importItems imports

importItems :: [ImportItem] -> ExceptT Text Agent ()
importItems imports =
 do let (actions, events) = foldr appendImp ([],[]) imports
    unless (null actions) $
        callServ (StoreActions actions) >>= convExceptT
    unless (null events) $
        callServ (ScheduleAddEvents events) >>= convExceptT
  where
    appendImp (ActionImport action') (actions,events) = (action' : actions, events)
    appendImp (EventImport event) (actions,events) = (actions, event : events)

loadImportFile :: FilePath -> ExceptT Text Agent [ImportItem]
loadImportFile fp =
 do esingle <- liftIO (decodeFileEither (convert fp))
    case esingle of
        Right (ActionImport action') ->
          do edecoded <- decodeComposite action'
             case edecoded of
                 Right decoded ->
                   return [ActionImport decoded]
                 Left msg -> throwE $ convert ("Could not decode: " ++ show action' ++ " : " ++ msg )
        Right single@(EventImport _) ->
          return [single]
        Left exc -> throwE (convert $ show exc)
