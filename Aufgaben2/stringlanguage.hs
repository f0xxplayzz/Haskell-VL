
type Value = String
type Variable = String
type Env = [(Variable, Value)]

data M a = Mon a deriving (Show)

class MyMonad m where 
 
 bind :: m a -> (a -> m b) -> m b 
 
 result :: a -> m a

-- Zustandsmonade
data M_ a = Mon_([(Variable,Value)] -> (a, [(Variable,Value)]))
getCont:: M_ a -> ([(Variable,Value)] -> (a, [(Variable,Value)]))
getCont (Mon_ transf) = transf

instance MyMonad (M) where
 result = \x -> Mon x
 bind (Mon x) f = (f x)
lookup':: Variable -> Env -> M_ Value
lookup' v' ((v, x): e) | v' == v = result x
 | otherwise = lookup' v' e
update :: Env -> Variable -> Value -> Env
update e v x = (v, x) : e
add :: Value -> Value -> M_ Value
add x y = Mon_ (\s->(x ++ y, s ))

instance MyMonad (M_) where
    result x = Mon_(\s->(x,s))
    bind(Mon_ transf) f = Mon_ (\s -> let (x',s') = transf s
        in getCont(f x') s')

data AExp = Var Variable
 | Const Value
 | Plus AExp AExp
 | Let Variable AExp AExp
eval :: AExp -> Env -> M_ Value
eval (Var v) e = lookup' v e
eval (Const c) e = result c
eval (Plus a1 a2) e = 
 bind (eval a1 e) 
 (\v1 -> (bind (eval a2 e) 
 (\v2 -> add v1 v2)))
eval (Let v a1 a2) e = 
 bind (eval a1 e)
 (\x -> eval a2 (update e v x))