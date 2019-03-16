data Shape = Circle Float | Rect Float Float

instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rect x y) = "Rectangle " ++ show x ++ " " ++ show y

square :: Float -> Shape
square x = Rect x x

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y
