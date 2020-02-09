-- Recursion on lists.
myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (n : ns) = n * myProduct ns

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

fastRev :: [a] -> [a]
fastRev xs = fastRevHelper xs []
             where fastRevHelper :: [a] -> [a] -> [a]
                   fastRevHelper [] acc = acc
                   fastRevHelper (x : xs) acc = fastRevHelper xs (x : acc)

myTake :: Int -> [a] -> [a]
myTake n xs = case (n, xs) of (0, _) -> []
                              (_, []) -> []
                              (n, x : xs) -> if n < 0 then [] else x : myTake (n - 1) xs

-- An implementation of insertion sort using recursion.
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys) | x <= y = x : y : ys
                  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

-- An implementation of quicksort using multiple recursion.
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
                 where smaller = [a | a <- xs, a <= x]
                       larger = [a | a <- xs, a > x]

-- Mutual recursion.
evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_ : xs) = evens xs

-- Exercises.
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n - 1) x

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x xs = if x == head xs then True else myElem x (tail xs)

-- An implementation of merge sort using recursion.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) | x < y = x : merge xs (y : ys)
                        | otherwise = y : merge (x : xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (take (length xs `div` 2) xs) (drop (length xs `div` 2) xs)
