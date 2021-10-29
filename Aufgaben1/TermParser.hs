import Aufgaben1.TermLexer
import Aufgaben1.Terme
import qualified GHC.Num as Float
import Aufgaben1.Kombinatoren

parser :: String -> Term
parser = fst . head . correctsols. parserMain . termLexer

parserMain::Parser TermToken Term
parserMain = parserMonom
    ||| parserAdd
    ||| parserDiv
    ||| parserMult


parserInt:: Parser TermToken Integer
parserInt (IntToken n:ts) = [(n,ts)]
parserInt _ = []

parserFloat:: Parser TermToken Float
parserFloat (FloatToken n:ts) = [(n,ts)]
parserFloat _ = []

parserMonom::Parser TermToken Term
parserMonom = ((parserFloat +.+ lexem XBASE +.+ lexem EXP +.+ parserInt)
        <<< \(((float,_),_),int) -> Monom float int)
    ||| ((parserInt +.+ lexem XBASE +.+ lexem EXP +.+ parserInt)
        <<< \(((a,_),_),int) -> Monom (Float.fromInteger a) int)
    ||| ((lexem BROPEN  +.+ parserFloat +.+ lexem XBASE +.+ lexem EXP +.+ parserInt +.+ lexem BRCLOSE)
        <<< \(((((_,a),_),_),int),_) -> Monom  a int)
    ||| ((lexem BROPEN  +.+ parserInt +.+ lexem XBASE +.+ lexem EXP +.+ parserInt +.+ lexem BRCLOSE)
        <<< \(((((_,a),_),_),int),_) -> Monom  (Float.fromInteger a) int)
    ||| ( (parserFloat +.+ lexem XBASE)
        <<< \(a,_) -> Monom a 1)
    ||| ( (parserInt +.+ lexem XBASE)
        <<< \(a,_) -> Monom (Float.fromInteger a) 1)
    ||| ( parserFloat
        <<< \a -> Monom a 0)
    ||| ( parserInt
        <<< \a -> Monom (Float.fromInteger a) 0)
    ||| ( lexem XBASE
        <<< \_ -> Monom 1 1)

parserAdd::Parser TermToken Term
parserAdd = ((parserMonom +.+ lexem PLUS +.+ parserMain)
        <<< \((a,_),b)-> (Add a b))
    ||| ((lexem BROPEN +.+ parserMain +.+ lexem PLUS +.+ parserMain +.+ lexem BRCLOSE)
        <<< \((((_,a),_),b),_) -> Add a b)

parserMult::Parser TermToken Term
parserMult = ((parserMonom +.+ (lexem STAR) +.+ parserMain)
        <<< \((a,_),b)-> (Mult a b))
    ||| ((lexem BROPEN +.+ parserMain +.+ lexem STAR +.+ parserMain +.+ lexem BRCLOSE)
        <<< \((((_,a),_),b),_) -> Mult a b)

parserDiv::Parser TermToken Term
parserDiv = ((parserMonom +.+ lexem SLASH +.+ parserMonom)
        <<< \((a,_),b)-> Div a b)
    ||| ((lexem BROPEN +.+ parserMain +.+ lexem SLASH +.+ parserMain +.+ lexem BRCLOSE)
        <<< \((((_,a),_),b),_) -> Div a b)
    ||| ((lexem BROPEN +.+ parserMain +.+ lexem BRCLOSE +.+ lexem SLASH +.+ lexem BROPEN +.+ parserMain +.+ lexem BRCLOSE)
        <<< \((((((_,a),_),_),_),b),_) -> Div a b)
    ||| ((lexem BROPEN +.+ parserMain +.+ lexem BRCLOSE +.+ lexem SLASH +.+ parserMonom)
        <<< \((((_,a),_),_),b) -> Div a b)