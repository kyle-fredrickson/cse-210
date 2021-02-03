module ParseArith
( ArithAST(..)
, ArithOps (..)
, arithExpr
, varStr
, arithParse
) where

import Data.Char ( isDigit, isAlpha, isAlphaNum )
import Prelude hiding ( lookup )
import Text.ParserCombinators.ReadP
    ( (+++), char, many1, option, readP_to_S, satisfy, skipSpaces, ReadP )

import Gen ( parens, brackets )

data ArithAST = ArithExpr {op :: ArithOps, l :: ArithAST, r :: ArithAST}
              | Var {v :: String}
              | Arr { v :: String, i :: ArithAST}
              | Int {n :: Integer}

data ArithOps = Add
              | Sub
              | Mul
              | Exp deriving Show

arithOps :: [(ArithOps, Char)]
arithOps = [
    (Add,'+'),
    (Sub,'-'),
    (Mul,'*'),
    (Exp,'^')
    ]

-- Parse --

arithParse :: String -> ArithAST
arithParse s = let (fst, snd) = (last . readP_to_S arithExpr) s
    in if snd == "" then
        fst
    else
        Int 0

arithExpr :: ReadP ArithAST
arithExpr = foldr (\(op,name) p ->
    let this = p +++ do a <- p +++ parens arithExpr
                        char name
                        ArithExpr op a <$> this
            in this)
        (number +++ var +++ arr +++ parens arithExpr)
            arithOps

arr ::  ReadP ArithAST
arr = do
    skipSpaces
    v <- varStr
    skipSpaces
    i <- brackets arithExpr
    skipSpaces
    return (Arr v i)

var :: ReadP ArithAST
var = do Var <$> varStr

varStr :: ReadP String
varStr = do
    skipSpaces
    f <- satisfy isAlpha
    v <- option "" (many1 (satisfy isAlphaNum))
    skipSpaces
    return (f : v)

number :: ReadP ArithAST
number = do
    skipSpaces
    minus <- option '0' (char '-')
    digits <- many1 (satisfy isDigit)
    skipSpaces
    return (Int (read (minus : digits)))

-- Debugging Tools --

instance Show ArithAST where
    show (Int n) = show n
    show (Var s) = s
    show (Arr v i) = show v ++ "[" ++ show i ++ "]"
    show (ArithExpr Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (ArithExpr Sub a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show (ArithExpr Mul a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
    show (ArithExpr Exp a b) = "(" ++ show a ++ "^" ++ show b ++ ")"