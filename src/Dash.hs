{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash(dashMain,doThing) where

import           BasicPrelude
import           Control.Concurrent
    (forkIO, threadDelay, threadCapability, myThreadId)
import           System.Process(readProcess)
import           Data.Text(pack)

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
