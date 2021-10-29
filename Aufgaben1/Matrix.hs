import Aufgaben1.Add

data Matrix = MAT [[Int]] 
    deriving(Show)

add_:: Matrix -> Matrix -> Matrix
addLines:: [Int]->[Int]->[Int]

addLines l1 l2 = zipWith (+) l1 l2
add_ (MAT m1) (MAT m2) = MAT $ zipWith addLines m1 m2

instance Aufgaben1.Add.Add Matrix where
    add [m1] = m1
    add (m1:m2:rest) = Aufgaben1.Add.add (add_ m1 m2:rest)