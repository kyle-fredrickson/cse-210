module Arith 
    (ArithAST, number, arith, arithParse, arithEval, parens) where 

import Data.Char ( isDigit )
import Text.ParserCombinators.ReadP
    ( (+++), ReadP, char, many1, option, readP_to_S, satisfy, skipSpaces )

data ArithAST = ArithExpr {op :: ArithOps, l :: ArithAST, r :: ArithAST}
              | Int {n :: Integer} deriving Show

data ArithOps = Add
              | Mul
              | Exp deriving Show

arithOps :: [(ArithOps, Char)]
arithOps = [
    (Add,'+'),
    (Mul,'*'),
    (Exp,'^')
    ]

arithEval :: ArithAST -> Integer
arithEval (Int n) = n
arithEval (ArithExpr Add l r) = arithEval l + arithEval r
arithEval (ArithExpr Mul l r) = arithEval l * arithEval r
arithEval (ArithExpr Exp l r) = arithEval l ^ arithEval r

arithParse :: String -> ArithAST
arithParse = fst . last . readP_to_S arith

arith :: ReadP ArithAST
arith = foldr (\(op,name) p ->
    let this = p +++ do a <- p +++ parens arith
                        char name
                        ArithExpr op a <$> this
            in this)
        (number +++ parens arith)
            arithOps

number :: ReadP ArithAST
number = do
    skipSpaces
    minus <- option '0' (char '-')
    digits <- many1 (satisfy isDigit)
    skipSpaces
    return (Int (read (minus : digits)))

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

-- Debugging Tools --

arithASTToStr :: ArithAST -> [Char]
arithASTToStr (Int n) = show n
arithASTToStr (ArithExpr Add l r) = "(+ " ++ arithASTToStr l ++ " " ++ arithASTToStr r ++ ")"
arithASTToStr (ArithExpr Mul l r) = "(* " ++ arithASTToStr l ++ " " ++ arithASTToStr r ++ ")"
arithASTToStr (ArithExpr Exp l r) = "(^ " ++ arithASTToStr l ++ " " ++ arithASTToStr r ++ ")"

main :: IO ()
main = do
    expr <- getLine
    (print . arithEval . arithParse) expr
