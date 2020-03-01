-- Functors.
data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

inc :: Functor f => f Int -> f Int
inc = fmap (+1)


-- Applicatives.
prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs <*> ys

getChars1 :: Int -> IO String
getChars1 0 = return []
getChars1 n = pure (:) <*> getChar <*> getChars1 (n - 1)

getChars2 :: Int -> IO String
getChars2 n = sequenceA $ replicate n getChar
