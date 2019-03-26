import System.IO

-- Derived primitives.
myGetLine :: IO String
myGetLine = do x <- getChar
               if x == '\n' then
                  return []
               else
                  do xs <- myGetLine
                     return (x:xs)

myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do putChar x
                     myPutStr xs

myPutStrLn :: String -> IO ()
myPutStrLn xs = do myPutStr xs
                   putChar '\n'

-- An interactive strlen.
strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters."

-- Hangman.
hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it!"
               else
                  do putStrLn (match word guess)
                     play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]
