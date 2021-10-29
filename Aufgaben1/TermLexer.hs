module Aufgaben1.TermLexer(termLexer,TermToken(..)) where

import Data.Char
import Graphics.Win32.GDI (restoreDC)
import Data.Data

data TermToken = FloatToken Float
    | IntToken Integer
    | XBASE
    | SLASH 
    | STAR 
    | PLUS
    | BROPEN 
    | BRCLOSE
    | EXP 
    deriving(Show,Eq)

termLexer:: String -> [TermToken]
numLexer:: String -> [TermToken]
floatReader::String -> [TermToken]
isPoint:: Char -> Bool
isFloat::Char -> Bool

termLexer [] = []
termLexer (c:cs)
    |isSpace c = termLexer cs
    |isDigit c = numLexer (c:cs)
termLexer('x':cs) = XBASE : termLexer cs
termLexer('/':cs) = SLASH : termLexer cs
termLexer('*':cs) = STAR : termLexer cs 
termLexer('+':cs) = PLUS : termLexer cs
termLexer('(':cs) = BROPEN : termLexer cs 
termLexer(')':cs) = BRCLOSE : termLexer cs 
termLexer('^':cs) = EXP : termLexer cs

isPoint c 
    | c =='.' = True 
    | otherwise = False

isFloat c = isDigit c || isPoint c

floatReader cs = FloatToken (read num) : termLexer rest
    where (num,rest) = span (isFloat) cs

numLexer cs = if isPoint f then floatReader cs else IntToken (read num) : termLexer (f:rest) 
    where (num,f:rest) = span isDigit cs