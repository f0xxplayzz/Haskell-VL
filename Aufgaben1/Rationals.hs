import GHC.Show (Show)
data Rational_ = RAT (Int,Int) deriving (Eq, Show)

add,sub,mul,div :: (Rational_,Rational_) -> Rational_
genGGT::Int -> Int -> Int

add (RAT(a,b),RAT(c,d)) = let 
    z=a*d+c*b 
    n = b*d
    ggt= genGGT z n
    in RAT(quot z ggt, quot n ggt)
sub (RAT(a,b),RAT(c,d)) =let 
    z=a*d-c*b
    n = b*d
    ggt= genGGT z n
    in RAT(quot z ggt, quot n ggt)
mul (RAT(a,b), RAT(c,d)) = let 
    z=a*c
    n = b*d
    ggt= genGGT z n
    in RAT(quot z ggt, quot n ggt)
div (RAT(a,b), RAT(c,d)) =let 
    z=a*d
    n = b*c
    ggt= genGGT z n
    in RAT(quot z ggt, quot n ggt)

genGGT a b | b>a = genGGT b a
    | otherwise = if mod a b == 0 then b else genGGT b (mod a b)
