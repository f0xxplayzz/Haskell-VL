data MoadicReadS a = Mon(String -> [(a,String)])

class MyMonad m where
    bind :: m a -> (a -> m b) -> m b
    result :: a -> m a

instance MyMonad M where
    result x = Mon (\s->(x,s))
    bind (Mon transf) f = Mon (\s-> let (x',s') = transf s
        in getCont (f x') s')
        

readMon :: Read a => MoadicReadS(Folge a)