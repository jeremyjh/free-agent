{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.Store(readCommand, writeCommand, getV, putV) where


import           BasicPrelude
import           Control.Monad
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Resource      (ResourceT, release)

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS

import           Data.Default                      (def)
import           Database.LevelDB

import qualified Dash.Proto                        as Proto
import qualified Dash.Proto.Runnable.NagiosCommand as NC

readCommand :: BS.ByteString -> IO NC.NagiosCommand
readCommand key = runResourceT $
    openDB >>= (\db -> get db def key) >>= parseCmd
  where
    parseCmd Nothing = error "Didn't find value for key"
    parseCmd (Just input) = case Proto.messageGet $ toLazy input of
        Right (cmd, remain) | LBS.length remain == 0 ->
            return cmd
        Right (_, _) ->
            error "Failed to parse Command fully."
        Left error_message ->
            error $ "Failed to parse Command." ++ error_message


writeCommand :: NC.NagiosCommand -> IO ()
writeCommand cmd = runResourceT $ do
    db <- openDB
    put db def
        (toStrict $ Proto.utf8 $ NC.host cmd)
        (toStrict $ Proto.messagePut cmd)
    return ()

putV :: BS.ByteString -> BS.ByteString -> IO ()
putV key value = runResourceT $ do
    db <- openDB
    put db def key value
    return ()

getV :: BS.ByteString -> IO (Maybe BS.ByteString)
getV key = runResourceT $ do
    db <- openDB
    get db def key

openDB :: ResourceT IO DB
openDB =  open "/tmp/leveltest"
    defaultOptions{createIfMissing = True, cacheSize= 2048}

toLazy :: BS.ByteString -> LBS.ByteString
toLazy strBS = LBS.fromChunks [strBS]

toStrict :: LBS.ByteString -> BS.ByteString
toStrict = BS.concat . LBS.toChunks
