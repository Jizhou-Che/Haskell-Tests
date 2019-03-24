-- Exercises.
myLast1 :: [a] -> a
myLast1 xs = head (drop (length xs - 1) xs)

myLast2 :: [a] -> a
myLast2 xs = xs !! (length xs - 1)

myInit1 :: [a] -> [a]
myInit1 xs = take (length xs - 1) xs

myInit2 :: [a] -> [a]
myInit2 xs = reverse (tail (reverse xs))
