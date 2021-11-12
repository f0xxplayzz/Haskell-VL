filterIntList:: [Int] ->(Int -> Int-> Bool) -> Int -> [Int]
filterIntList list func filter = [x |x <-list, func x filter]

filterString:: [Char] -> (Char->Char->Bool) -> Char -> [Char]
filterString list func filter = [x | x <-list, func x filter]

filterGenericList::(Ord a)=> [a] -> (a -> a -> Bool) -> a -> [a]
filterGenericList list func filter = [x | x <-list, func x filter]

filterGenericList_one_arg::(Ord a)=> [a] -> (a -> Bool) -> [a]
filterGenericList_one_arg list func = [x |  x <- list, func x]