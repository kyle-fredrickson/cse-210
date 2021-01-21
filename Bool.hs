module Bool
( BoolAST
, bool
, boolEval
, boolParse
, boolASTToStr
) where

import Control.Applicative ( Alternative((<|>)) )
import Data.Map ( Map , empty)
import Text.ParserCombinators.ReadP
    ( (+++), (<++), char, readP_to_S, skipSpaces, string, ReadP )

import Gen ( parens )
import Arith ( ArithAST, arithEval, arith, arithASTToStr )

data BoolAST = BoolExpr {op :: BoolOps, l :: BoolAST, r :: BoolAST}
             | ArithExpr {aop :: ArithOps, al :: ArithAST, ar :: ArithAST }
             | NotExpr {b :: BoolAST}
             | Bool {n :: Bool} deriving Show

data BoolOps = Or
             | And deriving Show

boolOps :: [(BoolOps, Char)]
boolOps = [
    (Or,'v'),
    (And,'^')
    ]

data ArithOps = Eq
              | Gt deriving Show

arithOps :: [(ArithOps, Char)]
arithOps = [
    (Eq,'='),
    (Gt,'>')
    ]

boolEval :: BoolAST -> Map String Integer -> Bool
boolEval (Bool n) _ = n
boolEval (NotExpr b) state = not (boolEval b state)
boolEval (BoolExpr Or l r) state = boolEval l state || boolEval r state
boolEval (BoolExpr And l r) state = boolEval l state && boolEval r state
boolEval (ArithExpr Eq l r) state = arithEval l state == arithEval r state
boolEval (ArithExpr Gt l r) state = arithEval l state > arithEval r state

-- Parse --

boolParse :: String -> BoolAST
boolParse = fst . last . readP_to_S bool

bool :: ReadP BoolAST
bool = foldr (\(op,name) p ->
    let this = p +++ do a <- p +++ parens bool
                        char name
                        BoolExpr op a <$> this
            in this)
        ((parens arithExpr <++ arithExpr <++ boolean) +++ (notExpr <++ parens bool))
            boolOps

arithExpr :: ReadP BoolAST
arithExpr = do
    l <- arith
    op <- char '=' <|> char '>'
    r <- arith
    if op == '=' then
        return (ArithExpr Eq l r)
    else
        return (ArithExpr Gt l r)

notExpr :: ReadP BoolAST
notExpr = do
    skipSpaces
    char '¬'
    skipSpaces
    NotExpr <$> bool

boolean :: ReadP BoolAST
boolean = do
    skipSpaces
    s <- string "true" <|> string "false"
    skipSpaces
    if s == "true" then
        return (Bool True)
    else
        return (Bool False)

-- Debugging Tools --

boolASTToStr :: BoolAST -> [Char]
boolASTToStr (Bool n) = show n
boolASTToStr (NotExpr b) = "(¬ " ++ boolASTToStr b ++ ")"
boolASTToStr (BoolExpr Or l r) = "(v " ++ boolASTToStr l ++ " " ++ boolASTToStr r ++ ")"
boolASTToStr (BoolExpr And l r) = "(^ " ++ boolASTToStr l ++ " " ++ boolASTToStr r ++ ")"
boolASTToStr (ArithExpr Eq l r) = "(= " ++ arithASTToStr l ++ " " ++ arithASTToStr r ++ ")"
boolASTToStr (ArithExpr Gt l r) = "(> " ++ arithASTToStr l ++ " " ++ arithASTToStr r ++ ")"

main :: IO ()
main = do
    expr <- getLine
    (print . ( `boolEval` empty) . boolParse) expr
