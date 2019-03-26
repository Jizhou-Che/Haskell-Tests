import Control.Monad
import Data.IORef

-- Type definition: a Counter is an IORef holding an Integer.
type Counter = IORef Integer

main :: IO ()
main = do
  -- Create a new reference with initial value of 0.
  c <- newIORef 0
  -- Read from the reference.
  n <- readIORef c
  putStrLn $ "Original counter value = " ++ show n
  -- Replicate an action n times and disgard the results.
  replicateM_ 10 (incCounter c)
  n <- readIORef c
  putStrLn $ "Final counter value = " ++ show n

incCounter :: Counter -> IO ()
incCounter c = do
  n <- readIORef c
  let n' = n + 1
  -- Write to the reference.
  writeIORef c n'
  putStrLn $ "New counter value = " ++ show n'
