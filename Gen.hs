module Gen ( parens, braces, brackets ) where

import Text.ParserCombinators.ReadP
    ( (+++), (<++), ReadP, char, string, many1, option, readP_to_S, satisfy, skipSpaces )
import Data.Map ( Map, lookup , empty)

parens :: ReadP b -> ReadP b
parens s = do
    skipSpaces
    paren1 <- char '('
    skipSpaces
    p <- s
    skipSpaces
    paren2 <- char ')'
    skipSpaces
    return p

braces :: ReadP b -> ReadP b
braces s = do
    skipSpaces
    paren1 <- char '{'
    skipSpaces
    p <- s
    skipSpaces
    paren2 <- char '}'
    skipSpaces
    return p

brackets:: ReadP b -> ReadP b
brackets s = do
    skipSpaces
    paren1 <- char '['
    skipSpaces
    p <- s
    skipSpaces
    paren2 <- char ']'
    skipSpaces
    return p