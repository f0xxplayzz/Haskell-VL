module Aufgaben1.Terme(Term(..)) where

import qualified GHC.Num as Float
import qualified System.Directory.Internal.Prelude as Float
data Term = Monom Float Integer
    |Add Term Term
    |Mult Term Term
    |Div Term Term
    deriving(Eq)

instance Num Term where
    (+) (Monom a b) (Monom c d)
        |b==d &&d==1 = Monom (a+ (Float.fromInteger b)) 1
        |otherwise = Monom (a^b+c^d) 1
    (+) (Div a b) (Div c d)
        |c /= d = Div (a*d+c*b) (b*c)
        | otherwise = Div (a+c) b
    (+) a b = Add a b
    (*) (Div a b) (Div c d) = Div (a*c) (b*d)
    (*) (Monom a b) (Monom c d) | a==c && b==d = Monom (a*c) b
        | a == c = Monom a (b+d)
        | b==d = Monom (a*c) d
        | otherwise = Monom ((a^b)*(c^d)) 1
    (*) a b = Mult a b
    (-) (Monom a b) (Monom c d)
        |a==Float.fromInteger d && d==1 = Monom (a-Float.fromInteger b) 1
        |b<0 && d>0 = (+) (Monom a (-b)) (Monom c d)
        |b>0 && d<0 = (+) (Monom a b) (Monom c (-d))
        |otherwise = Monom (a^b-c^d) 1

    fromInteger a = Monom (Float.fromInteger a) 1
    negate (Monom a b) = Monom (-a^b) 1
    signum a = Monom 0 1
    abs a = Monom 1 1

instance Show Term where
    show (Mult (Monom a b)(Monom c d)) = show (Monom (a*c) (b+d)) 
    show (Mult (Add a b) (Add c d)) = show (Mult a c) ++ "+" ++ show (Mult a d) ++ "+" ++ show (Mult b c) ++ "+" ++ show (Mult b d)
    show (Mult (Add a b) c) = show (Mult a c) ++ "+" ++ show (Mult b c)
    show (Mult c (Add a b)) = show (Mult a c) ++ "+" ++ show (Mult b c)
    show (Monom a b)|a==0 = "0"
        |b==0 = show a
        |b==1 = show a ++ "x"
        |otherwise = show a ++ "x^" ++ show b
    show (Add (Monom a 0) (Monom b 0)) = show (a+b)
    show (Add a b) = show a ++ "+" ++ show b 
    show (Div a b) = "(" ++ show a ++ ")/(" ++ show b ++ ")"
