import Control.Monad.State
type Stack a = [a]

push :: a -> State ( Stack a ) ()
push a = state $ \xs -> ((),a:xs)

top:: State (Stack a) a
top = state $ \(xs)-> (head xs,tail xs)


is_empty:: State (Stack a) Bool
is_empty = state $ \xs -> (null xs,xs)

emptySt:: State (Stack a) ()
emptySt = state $ \_ -> ((),[])

bracketKernel:: [Char] -> State (Stack Char) Bool
bracketKernel (c:cs) = do
    if c == '(' then 
        do
        push c
        bracketKernel cs
    else
        do
        emp <- is_empty
        if not emp then
            do
                check <- top
                if check == '(' then
                    do
                    bracketKernel cs
                else 
                    do
                    is_empty
            else
                do 
                state $ \s -> (False,s)
bracketKernel [] = do
    is_empty

isBracket :: Char -> Bool 
isBracket c
    | c == ')' = True
    | c == '(' = True
    | otherwise = False

bracketsCheck:: [Char] -> Bool 
bracketsCheck chars = evalState (bracketKernel [c | c<-chars,isBracket c]) []