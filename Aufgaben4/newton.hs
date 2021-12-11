data Term   = Monom(Float, Integer)
            | Addition(Term, Term) 
            | Multiplikation(Term, Term) 
            | Division(Term, Term) deriving(Show)

diff :: Term -> Term
diff(Monom(a,0)) = Monom(0,0)
diff(Monom(a,b)) = Monom(a*fromInteger(b),b-1)
diff(Addition(a,b)) = Addition(diff(a),diff(b))
diff(Multiplikation(a,b)) = Addition(Multiplikation(diff(a),b),Multiplikation(a,diff(b)))
diff(Division(a,b)) = Division(Addition(Multiplikation(diff(a),b),Multiplikation(Monom(-1,0),Multiplikation(a,diff(b)))),Multiplikation(b,b))

transform::Term ->(Float -> Float)
transform (Monom(a,0)) = \_-> a
transform (Monom(a,b)) = \x->a*(x^b)
transform(Addition(a,b)) = \x-> (transform a x) + (transform b x)
transform(Multiplikation(a,b))= \x->(transform a x) * (transform b x)
transform(Division(a,b))= \x->(transform a x) / (transform b x)

step1::(Float->Float)->(Float->Float)->Float->Float
step1 f f' a=a-(f a)/(f' a)

stop1::Float->[Float]->Float
stop1 eps (a1:(a2:as))
    |abs(a1-a2)<eps = a2
    |otherwise =stop1 eps (a2:as)

newton::Term->Float->Float->Float
newton f a0 eps = stop1 eps (iterate (step1 (transform f) (transform (diff f))) a0)