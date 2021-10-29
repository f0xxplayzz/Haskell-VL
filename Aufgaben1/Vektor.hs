import Aufgaben1.Add
import GHC.Generics (V1)

data Vektor = VEK [Int]

add_:: Vektor -> Vektor -> Vektor

add_ (VEK v1) (VEK v2) = VEK $ zipWith (+) v1 v2

instance Add Vektor where
    add [v1] = v1 
    add (v1:v2:rest) = add  $ add_ v1 v2:rest