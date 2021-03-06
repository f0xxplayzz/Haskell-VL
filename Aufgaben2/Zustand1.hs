import Data.Binary.Get (lookAheadM)
type Value = Int
type Variable = String
type Env = [(Variable,Value)]
data AExp = Var Variable
    | Const Value
    | Plus AExp AExp
    | Let Variable AExp AExp

eval :: AExp -> Env -> M Value
eval (Var v) e = result $ lookup' v e
eval (Const c) e = result c
eval (Plus a1 a2) e  =
    bind (eval a1 e)
        (\v1 -> bind (eval a2 e)
            (\v2 -> add v1 v2))
eval (Let v a1 a2) e  =
    bind (eval a1 e)
        (\x -> Mon(\env -> (x,(update e v x))))
class MyMonad m where
    bind :: m a -> (a -> m b) -> m b
    result :: a -> m a

data M a = Mon ([(Variable,Value)] -> (a, [(Variable,Value)]))

getCont:: M a -> ([(Variable,Value)] -> (a, [(Variable,Value)]))
getCont (Mon transf) = transf
instance MyMonad M where
    result x = Mon (\s->(x,s))
    bind (Mon transf) f = Mon (\s-> let (x',s') = transf s
        in getCont (f x') s')

lookup':: Variable -> Env -> Value
lookup' v' ((v, x): e) | v' == v = x
    | otherwise = lookup' v' e
update :: Env -> Variable -> Value -> Env
update e v x = (v, x) : e

add:: Value -> Value -> M Value
add x y = Mon (\s->( x + y, s ))