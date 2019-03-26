import System.IO

main :: IO ()
main = do
  putStr "File name: "
  fileName <- getLine
  putStr "File content: "
  fileContent <- getLine
  writeToFile fileName fileContent
  readFromFile fileName

writeToFile :: String -> String -> IO ()
writeToFile fileName fileContent = do
  -- Open a file and return the file handle.
  hdl <- openFile fileName WriteMode
  -- Write to the file.
  hPutStrLn hdl fileContent
  -- Close the file.
  hClose hdl

readFromFile :: String -> IO ()
readFromFile fileName = do
  -- Read from file.
  str <- readFile fileName
  putStrLn $ "Content of file '" ++ fileName ++ "':\n" ++ str
