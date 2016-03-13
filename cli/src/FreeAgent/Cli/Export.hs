{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module FreeAgent.Cli.Export
    (exportActions) where

import FreeAgent.AgentPrelude
import FreeAgent.Core                (Agent, key)
import FreeAgent.Core.Protocol.Executive    (QueryActions(..))
import FreeAgent.Core.Protocol.Schedule     (ScheduleQueryEvents(..))
import FreeAgent.Core.Protocol (callServ)
import FreeAgent.Cli.Import

import qualified Filesystem.Path as Path
import Control.Error                 (throwE)
import Data.EitherR                  (runExceptRT, succeedT)
import Data.Yaml                     (encodeFile)
import Shelly                        (shelly, test_d, test_f)


selectMode :: (MonadIO m) => FilePath -> m (Either Text Mode)
selectMode fp = shelly . runExceptT . runExceptRT $
  do isFile <- lift $ test_f fp
     when isFile (succeedT File)
     isDir <- lift $ test_d fp
     when isDir (succeedT Dir)
     hasParent <- lift $ test_d $ Path.directory fp
     when hasParent (succeedT File)
     return "Path not found."

exportActions :: FilePath -> Agent (Either Text ())
exportActions fp =
  runExceptT $
   do mode <- selectMode fp
      case mode of
          Right File -> exportFile fp
          Right Dir -> exportDirectory fp
          Left reason -> throwE reason
      return ()

exportFile :: FilePath -> ExceptT Text Agent ()
exportFile fp =
 do imports <- fetchItems
    case imports of
       []       -> throwE "Nothing to export."
       imports' -> liftIO $ encodeFile (convert fp) imports'

exportDirectory :: FilePath -> ExceptT Text Agent ()
exportDirectory fp =
 do imports <- fetchItems
    case imports of
       []       -> throwE "Nothing to export."
       imports'-> liftIO $
           forM_ imports' $ \ import'->
               let file = fp </> convert (itemKey import' ++ ".yaml")
               in encodeFile (convert file) import'
  where
    itemKey (ActionImport a) = key a
    itemKey (EventImport e) = key e

fetchItems :: ExceptT Text Agent [ImportItem]
fetchItems =
 do actions <- callServ QueryActions >>= convExceptT
    events <- callServ ScheduleQueryEvents >>= convExceptT
    let aimports = foldr (\x xs -> ActionImport x : xs) [] actions
    return $
        foldr (\x xs -> EventImport x : xs) aimports events
