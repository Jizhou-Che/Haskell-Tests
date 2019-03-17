myAbs1 :: Int -> Int
myAbs1 n = if n >= 0 then n else -n

myAbs2 :: Int -> Int
myAbs2 n | n >= 0 = n
         | otherwise = -n

mySignum1 :: Int -> Int
mySignum1 n = if n < 0 then -1 else
                 if n == 0 then 0 else 1

mySignum2 :: Int -> Int
mySignum2 n | n < 0 = -1
            | n == 0 = 0
            | otherwise = 1

myHead :: [a] -> a
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

myAdd1 :: Int -> Int -> Int
myAdd1 x y = x + y

myAdd2 :: Int -> Int -> Int
myAdd2 = \x y -> x + y

myAdd3 :: Int -> Int -> Int
myAdd3 x = \y -> x + y

myAdd4 :: Int -> Int -> Int
myAdd4 = \x -> (\y -> x + y)

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0..n - 1]

mySum :: [Int] -> Int
mySum = foldl (+) 0

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs
