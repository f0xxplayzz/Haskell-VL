data K1 a b = Con11 a | Con12 a (K2 b)
data K2 a = Con21 | Con22 a
f:: Int -> Int -> (K1 Int Int) -> Int
f x y (Con11 n) = x * y * n
f x y (Con12 n Con21) = x * y * n
f x y (Con12 n (Con22 m)) = x * y * n * 2 * m
f x y (Con12 n a) = x * y * n
g:: a -> a
g x = g x
