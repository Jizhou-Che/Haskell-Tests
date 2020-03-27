import Data.Char


-- Functors.

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

inc :: Functor f => f Int -> f Int
inc = fmap (+ 1)


-- Applicatives.

prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs <*> ys

getChars1 :: Int -> IO String
getChars1 0 = return []
getChars1 n = pure (:) <*> getChar <*> getChars1 (n - 1)

getChars2 :: Int -> IO String
getChars2 n = sequenceA $ replicate n getChar


-- Monads.

data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval1 :: Expr -> Maybe Int
eval1 (Val n) = Just n
eval1 (Div x y) = eval1 x >>= \n ->
                  eval1 y >>= \m ->
                  safediv n m

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = do
  n <- eval2 x
  m <- eval2 y
  safediv n m

myPairs1 :: [a] -> [b] -> [(a, b)]
myPairs1 xs ys = [(x, y) | x <- xs, y <- ys]

myPairs2 :: [a] -> [b] -> [(a, b)]
myPairs2 xs ys = do
  x <- xs
  y <- ys
  return (x, y)

myPairs3 :: [a] -> [b] -> [(a, b)]
myPairs3 xs ys = do
  x <- xs
  y <- ys
  [(x, y)]


-- The state monad.

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s -> let (f, s') = app stf s
                             (x, s'') = app stx s'
                             in (f x, s''))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')


-- Relabelling trees.

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
                      where (l', n') = rlabel l n
                            (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
  n <- fresh
  return (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')

label1 :: Tree a -> Tree Int
label1 t = fst $ rlabel t 0

label2 :: Tree a -> Tree Int
label2 t = fst $ app (alabel t) 0

label3 :: Tree a -> Tree Int
label3 t = fst $ app (mlabel t) 0


-- Generic functions.

myMapM :: Monad m => (a -> m b) -> [a] -> m [b]
myMapM _ [] = return []
myMapM f (x : xs) = do
  y <- f x
  ys <- myMapM f xs
  return (y : ys)

charToDigit :: Char -> Maybe Int
charToDigit c | isDigit c = Just (digitToInt c)
              | otherwise = Nothing

stringToDigits :: String -> Maybe [Int]
stringToDigits = myMapM charToDigit

myFilterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
myFilterM _ [] = return []
myFilterM p (x : xs) = do
  b <- p x
  ys <- myFilterM p xs
  return (if b then x : ys else ys)

powerset1 :: [a] -> [[a]]
powerset1 [] = [[]]
powerset1 (x : xs) = [x : ps | ps <- powerset1 xs] ++ powerset1 xs

powerset2 :: [a] -> [[a]]
powerset2 = myFilterM (\x -> [True, False])

join1 :: Monad m => m (m a) -> m a
join1 mmx = do
  mx <- mmx
  x <- mx
  return x

join2 :: Monad m => m (m a) -> m a
join2 x = x >>= id


-- Exercises.

data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a)
               deriving Show

instance Functor Tree' where
  -- fmap :: (a -> b) -> Tree' a -> Tree' b
  fmap _ Leaf' = Leaf'
  fmap g (Node' l x r) = Node' (fmap g l) (g x) (fmap g r)

newtype ZipList a = Z [a]
                    deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)
  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  Z gs <*> Z xs = Z [g x | (g, x) <- zip gs xs]

instance Monad ZipList where
  -- (>>=) :: ZipList a -> (a -> ZipList b) -> ZipList b
  Z xs >>= f = Z [y | Z ys <- map f xs, y <- ys]
