-- Type declarations.
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> [v]
find k t = [v | (k', v) <- t, k == k']

-- Data declarations.
data Shape = Circle Float | Rect Float Float

instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rect x y) = "Rectangle " ++ show x ++ " " ++ show y

square :: Float -> Shape
square x = Rect x x

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

-- Maybe :: * -> *.
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- Recursive types.
data Nat = Zero | Succ Nat
           deriving (Show, Eq, Ord)

instance Enum Nat where
  fromEnum Zero = 0
  fromEnum (Succ n) = 1 + fromEnum n
  toEnum 0 = Zero
  toEnum n = Succ (toEnum (n - 1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

natAdd :: Nat -> Nat -> Nat
natAdd Zero n = n
natAdd (Succ m) n = Succ (natAdd m n)

-- A recursive definition of lists.
data List a = Nil | Cons a (List a)
              deriving Show

instance Foldable List where
  foldr _ v Nil = v
  foldr f v (Cons x xs) = f x (foldr f v xs)

instance Eq a => Eq (List a) where
  (==) Nil Nil = True
  (==) (Cons x xs) (Cons y ys) = x == y && xs == ys
  (==) _ _ = False

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

-- A recursive difinition of binary trees.
data Tree a = Leaf a | Node (Tree a) a (Tree a)
              deriving Show

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
