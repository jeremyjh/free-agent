{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash(dashMain,doThing, readCommand, writeCommand) where

import           BasicPrelude
import           Control.Concurrent
    (forkIO, threadDelay, threadCapability, myThreadId)
import           System.Process(readProcess)
import           Data.Text(pack)
import qualified Dash.Proto as Proto
import qualified Dash.Proto.Runnable.NagiosCommand as NC
import qualified Data.ByteString.Lazy as LByteS

dashMain :: IO ()
dashMain = do
    void $ forkIO (doThing >>= putStrLn)
    void $ forkIO saySomething
    delaySeconds 1
    saySomething
  where
    delaySeconds n = threadDelay (n * 1000000)
    saySomething = do
        myThreadId >>= threadCapability >>= print
        putStrLn "I just want to say"
        delaySeconds 2
        putStrLn "Something"

doThing :: IO Text
doThing = pack <$> readProcess "./thing.sh" [] []

readCommand :: String -> IO NC.NagiosCommand
readCommand file = LByteS.readFile file >>= parseCmd
  where
    parseCmd input = case Proto.messageGet input of
        Right (cmd, remain) | LByteS.length remain == 0 ->
            return cmd
        Right (_, _) ->
            error $ "Failed to parse command fully."
        Left error_message ->
            error $ "Failed to parse command." ++ error_message


writeCommand :: String -> NC.NagiosCommand -> IO ()
writeCommand file cmd = LByteS.writeFile file $ (Proto.messagePut cmd)
