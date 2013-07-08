{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash.Store(readCommand, writeCommand, getV, putV, openDB) where


import           BasicPrelude
import qualified Prelude as P
import           Control.Monad
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Resource      (release, ResIO)

import qualified Data.ByteString.Lazy              as LByteS

import           Data.Default                      (def)
import           Database.LevelDB

import qualified Dash.Proto                        as Proto
import qualified Dash.Proto.Runnable.NagiosCommand as NC

readCommand :: DB -> ByteString -> ResIO NC.NagiosCommand
readCommand db key =
    get db def key >>= parseCmd
  where
    parseCmd Nothing = error "Didn't find value for key"
    parseCmd (Just input) = case Proto.messageGet $ toLazy input of
        Right (cmd, remain) | LByteS.length remain == 0 ->
            return cmd
        Right (_, _) ->
            error "Failed to parse Command fully."
        Left error_message ->
            error $ "Failed to parse Command." ++ error_message


writeCommand :: DB -> NC.NagiosCommand -> ResIO ()
writeCommand db cmd = do
    put db def
        (toStrict $ Proto.utf8 $ NC.host cmd)
        (toStrict $ Proto.messagePut cmd)
    return ()

putV :: DB -> ByteString -> ByteString -> ResIO ()
putV db key value = put db def key value

getV :: DB -> ByteString -> ResIO (Maybe ByteString)
getV db key = get db def key

openDB :: P.FilePath -> ResIO DB
openDB path =  open path
    defaultOptions{createIfMissing = True, cacheSize= 2048}

toLazy :: ByteString -> LByteString
toLazy bs = LByteS.fromChunks [bs]

toStrict :: LByteString -> ByteString
toStrict = concat . LByteS.toChunks
