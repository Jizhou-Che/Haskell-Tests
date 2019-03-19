myMap1 :: (a -> b) -> [a] -> [b]
myMap1 f xs = [f x | x <- xs]

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 _ [] = []
myMap2 f (x:xs) = f x : myMap2 f xs

myMap3 :: (a -> b) -> [a] -> [b]
myMap3 f = foldr (\x xs -> f x : xs) []

myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 p xs = [x | x <- xs, p x]

myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 _ [] = []
myFilter2 p (x:xs) | p x = x : myFilter2 p xs
                   | otherwise = myFilter2 p xs

myFilter3 :: (a -> Bool) -> [a] -> [a]
myFilter3 p = foldr (\x xs -> if p x then x : xs else xs) []

mySum :: Num a => [a] -> a
mySum = foldr (+) 0

myProduct :: Num a => [a] -> a
myProduct = foldr (*) 1

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f v [] = v
myFoldr f v (x:xs) = f x (myFoldr f v xs)

myLength1 :: [a] -> Int
myLength1 = foldr (\_ n -> 1 + n) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

myReverse1 :: [a] -> [a]
myReverse1 = foldr snoc []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f v [] = v
myFoldl f v (x:xs) = myFoldl f (f v x) xs

myLength2 :: [a] -> Int
myLength2 = foldl (\n _ -> n + 1) 0

myReverse2 :: [a] -> [a]
myReverse2 = foldl (\xs x -> x : xs) []

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
