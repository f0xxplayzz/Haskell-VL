setSort::[Int]->[Int]->[Int]
setSort [] y = y
setSort x [] = x
setSort (x:xs) (y:ys)
    |x==y = x:setSort xs ys
    |x<y = x:setSort xs (y:ys)
    |otherwise = y:setSort (x:xs) ys

getEntries::Int->[Int]->[Int]
getEntries 0 _ = []
getEntries a xs = head xs:(getEntries (a-1) (tail xs))

hamming::Int -> Int -> Int -> [Int]
hamming a b c= 1:setSort (map (\x->x*a) (hamming a b c))
    (setSort (map (\y->y*b) (hamming a b c))
    (map (\z->z*c) (hamming a b c)))