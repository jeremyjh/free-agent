module Dash(dashMain,doThing) where

import Control.Concurrent(forkIO, threadDelay, threadCapability, myThreadId)
import System.Process(readProcess)

dashMain :: IO ()
dashMain =
  forkIO (doThing >>= putStrLn)
  >> forkIO saySomething
  >> delaySeconds 1
  >> saySomething
  where
    delaySeconds n = threadDelay (n * 1000000)
    saySomething =
      myThreadId >>= threadCapability
      >>= print 
      >> putStrLn "I just want to say"
      >> delaySeconds 2
      >> putStrLn "Something"

doThing :: IO String
doThing = readProcess "./thing.sh" [] []
    
