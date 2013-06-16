import Control.Concurrent(forkIO, threadDelay, threadCapability, myThreadId)
import System.Process(readProcess)
import System.IO(hFlush, stdout)

main :: IO ()
main =
  forkIO doThing
  >> forkIO saySomething
  >> delaySeconds 1
  >> saySomething
  where
    doThing = readProcess "./thing.sh" [] [] >>= putStrLn
    delaySeconds n = threadDelay (n * 1000000)
    saySomething =
      myThreadId >>= threadCapability
      >>= putStrLnFl . show
      >> putStrLnFl "I just want to say"
      >> delaySeconds 2
      >> putStrLnFl "Something"

putStrLnFl :: String -> IO ()
putStrLnFl s = putStrLn s >> hFlush stdout
