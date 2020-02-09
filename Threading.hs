import Control.Concurrent
import Control.Monad
import System.IO
import System.Random

main :: IO ()
main = do
  -- Set the output buffering to LineBuffering to make output readable.
  hSetBuffering stdout LineBuffering
  putStrLn "MAIN: Creating threads..."
  -- Start threads using forkIO.
  forM_ [1 .. 7] (forkIO . helloThread)
  -- Main thread waits forever.
  forever (threadDelay 1000000)

helloThread :: Int -> IO ()
helloThread i = forever $ do
  putStrLn $ "THREAD " ++ show i ++ ": Hello, World!"
  -- Random wait between 100 and 1000 ms.
  delay <- randomRIO (100000, 1000000)
  threadDelay delay
