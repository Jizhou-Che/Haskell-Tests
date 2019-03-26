import Control.Concurrent
import Control.Monad
import System.Random
import System.IO

numConsumers :: Int
numConsumers = 7

main :: IO ()
main = do
  -- Set the output buffering to LineBuffering to make output readable.
  hSetBuffering stdout LineBuffering
  -- Create a new EMPTY mutable location (MVar).
  -- Taking a value from an empty MVar blocks the thread, putting a value into a full MVar blocks the thread.
  box <- newEmptyMVar
  putStrLn $ "MAIN: Creating " ++ show numConsumers ++ " consumers..."
  -- Create the consumer threads.
  forM_ [1..numConsumers] (forkIO . consumer box)
  putStrLn "MAIN: Creating producer..."
  -- Start producing in the main thread forever.
  producer box 0

producer :: MVar Integer -> Integer -> IO ()
producer box n = do
  -- Put the next value into the MVAR.
  -- If it is still full, then the thread (main) will block until a consumer has taken the value from the MVar.
  putMVar box n
  putStrLn $ "PRODUCER: Filled box with " ++ show n
  -- Tail-recursive call to produce the next value.
  -- Important to be tail-recursive because this runs potentially forever.
  producer box (n + 1)

consumer :: MVar Integer -> Int -> IO ()
consumer box i = forever $ do
  putStrLn $ "CONSUMER " ++ show i ++ ": Waiting for box to be filled..."
  -- Try to take the content of the MVar.
  -- If its empty, block and wait until the producer has put a value in it.
  n <- takeMVar box
  putStrLn $ "CONSUMER " ++ show i ++ ": Consumed content of box = " ++ show n ++ "."
  -- Randomly wait between 1 and 10 seconds.
  delay <- randomRIO (1000000, 10000000)
  threadDelay delay
