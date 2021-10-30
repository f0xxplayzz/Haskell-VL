module Aufgaben1.Kombinatoren where


type Parser tok a = [tok] -> [(a, [tok])]

--aufgerufen wenn Fehler
failure :: Parser a b
failure _ = []

--Sprache des leeren Wortes
succeed :: a -> Parser tok a
succeed value toks = [(value, toks)]

--bedingte Erkennung
satisfy :: (tok -> Bool) -> Parser tok tok
satisfy cond [] = []
satisfy cond (tok : toks) | cond tok = succeed tok toks
    | otherwise = failure toks

--erkennen eines bestimmten Lexems bzw. eines bestimmten Terminalsymbols
lexem :: Eq tok => tok -> Parser tok tok
lexem tok toks = satisfy ((==) tok) toks

--nacheinander erkennen ausgabe von Parser1 in Parser2 ... 
(+.+) :: Parser tok a -> Parser tok b -> Parser tok (a,b)
(p1 +.+ p2) toks = [((v1, v2), rest2)
    | (v1, rest1) <- p1 toks,(v2, rest2) <- p2 rest1]

--Alternative Verbinde Ausgabe von Parser1 mit Ausgabe von Parser2
(|||) :: Parser tok a -> Parser tok a -> Parser tok a
(p1 ||| p2) toks = p1 toks ++ p2 toks

--Transformation
(<<<) :: Parser tok a -> (a -> b) -> Parser tok b
(p <<< f) toks = [ (f v, rest)
    | (v, rest) <- p toks]

--nur korrekte abstrakte Syntax
correctsols :: [(t, [a])] -> [(t, [a])]
correctsols sols = (filter (\(_, resttokens) -> null resttokens)) sols