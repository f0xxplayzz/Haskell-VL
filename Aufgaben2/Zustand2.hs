type Value = Int
type Variable = String
type Env = [(Variable,Value)]
data AExp = Var Variable
    | Const Value
    | Plus AExp AExp
    | Let Variable AExp AExp

eval :: AExp -> M Value
eval (Var v) = Mon (\e -> (lookup' v e,e))
eval (Const c)  = result c
eval (Plus a1 a2)  =
    bind (eval a1)
        (\v1 -> bind (eval a2)
            (\v2 -> add v1 v2))
eval (Let v a1 a2)  =
    bind (
        bind (
            bind (eval a1) 
                (\x->Mon(\e->(x, update e v x)))
            )
            (\_ -> eval a2)
    ) 
    (\x -> Mon(\(h:t) -> (x, t)))
class MyMonad m where
    bind :: m a -> (a -> m b) -> m b
    result :: a -> m a

data M a = Mon (Env -> (a, Env))

getCont:: M a -> (Env -> (a, Env))
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