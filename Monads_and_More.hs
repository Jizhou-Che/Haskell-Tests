import Data.Char
import Control.Monad
import Control.Monad.Trans.Class


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


-- Monad transformers.

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
  -- fmap :: (Monad m) => (a -> b) -> MaybeT m a -> MaybeT m b
  fmap = liftM

instance Monad m => Applicative (MaybeT m) where
  -- pure :: (Monad m) => a -> MaybeT m a
  pure = return
  -- (<*>) :: (Monad m) => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (<*>) = ap

instance Monad m => Monad (MaybeT m) where
  -- return :: (Monad m) => a -> MaybeT m a
  return = MaybeT . return . Just
  -- (>>=) :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $ do
    mv <- runMaybeT x
    case mv of Nothing -> return Nothing
               Just v -> runMaybeT (f v)

instance MonadTrans MaybeT where
  -- lift :: (Monad m) => m a -> MaybeT m a
  lift = MaybeT . (liftM Just)


-- The writer monad.

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Monoid w => Functor (Writer w) where
  -- fmap :: (Monoid w) => (a -> b) -> Writer w a -> Writer w b
  fmap g (Writer (x, v)) = Writer (g x, v)

instance Monoid w => Applicative (Writer w) where
  -- pure :: (Monoid w) => a -> Writer w a
  pure x = Writer (x, mempty)
  -- (<*>) :: (Monoid w) => Writer w (a -> b) -> Writer w a -> Writer w b
  Writer (g, v1) <*> Writer (x, v2) = Writer (g x, mappend v1 v2)

instance Monoid w => Monad (Writer w) where
  -- (>>=) :: (Monoid w) => Writer w a -> (a -> Writer w b) -> Writer w b
  Writer (x, v1) >>= f = let Writer (y, v2) = f x in Writer (y, mappend v1 v2)

newtype WriterT w m a = WriterT { runWriterT :: m (Writer w a) }

instance (Monad m, Monoid w) => Functor (WriterT w m) where
  -- fmap :: (Monoid w, Monad m) => (a -> b) -> WriterT w m a -> WriterT w m b
  fmap = liftM

instance (Monad m, Monoid w) => Applicative (WriterT w m) where
  -- pure :: (Monoid w, Monad m) => a -> WriterT w m a
  pure = return
  -- (<*>) :: (Monoid w, Monad m) => WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
  (<*>) = ap

instance (Monad m, Monoid w) => Monad (WriterT w m) where
  -- return :: (Monoid w, Monad m) => a -> WriterT w m a
  return = WriterT . return . return
  -- (>>=) :: (Monoid w, Monad m) => WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  x >>= f = WriterT $ do
    w1 <- runWriterT x
    let (y, l1) = runWriter w1
    w2 <- runWriterT (f y)
    let (z, l2) = runWriter w2
    return $ Writer (z, mappend l1 l2)

instance Monoid w => MonadTrans (WriterT w) where
  -- lift :: (Monoid w, Monad m) => m a -> WriterT w m a
  lift = WriterT . (liftM return)
