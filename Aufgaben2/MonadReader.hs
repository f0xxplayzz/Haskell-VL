data MoadicReadS a = Mon(String -> [(a,String)])

data Folge a = Empty | Cons(a, Folge a) deriving(Show)
class MyMonad m where
    bind :: m a -> (a -> m b) -> m b
    result :: a -> m a

instance MyMonad MoadicReadS where
    result x = Mon (\s->[(x,s)])
    bind (Mon transf) f = Mon (\s-> let [(x',s')] = transf s
        in getCont (f x') s')
        
instance Read a => Read(Folge a) where
    readsPrec _ inp = let [(a,b)] = lex inp in
        case a of
            "[" -> readsPrec 0 b
            "]" -> readsPrec 0 b
            "," -> readsPrec 0 b
            " " -> readsPrec 0 b
            "" -> [(Empty,"")]
            c -> let [(res,"")] = readsPrec 0 b in
                [((Cons(read c,res)),"")]

getCont::MoadicReadS a -> (String->[(a,String)])
getCont (Mon a)  = a

readMon:: Read a => MoadicReadS(Folge a)
readMon = bind
        (Mon (\inp -> let [(a,b)] = lex inp in
        case a of
            "[" -> getCont readMon b
            "]" -> getCont readMon b
            "," -> getCont readMon b
            " " -> getCont readMon b
            "" -> [(Empty,"")]
            c -> let [(res,"")] = getCont readMon b in
                [((Cons(read c,res)),"")]))
        (\s -> Mon(\_ -> [(s,"")]) )