module Bool
( BoolAST
, boolExpr
, boolEval
, boolParse
, boolASTToStr
) where

import Control.Applicative ( Alternative((<|>)) )
import Data.Map ( Map , empty)
import Text.ParserCombinators.ReadP
    ( (+++), (<++), char, readP_to_S, skipSpaces, string, ReadP )

import Gen ( parens )
import Arith ( ArithAST, arithEval, arithExpr, arithASTToStr )

data BoolAST = BoolExpr {op :: BoolOps, l :: BoolAST, r :: BoolAST}
             | ArithExpr {aop :: ArithOps, al :: ArithAST, ar :: ArithAST }
             | NotExpr {b :: BoolAST}
             | Bool {n :: Bool} deriving Show

data BoolOps = Or
             | And deriving Show

boolOps :: [(BoolOps, Char)]
boolOps = [
    (Or,'∨'),
    (And,'∧')
    ]

data ArithOps = Eq
              | Lt deriving Show

arithOps :: [(ArithOps, Char)]
arithOps = [
    (Eq,'='),
    (Lt,'<')
    ]

-- Eval --

boolEval :: BoolAST -> Map String Integer -> Bool
boolEval (Bool n) _ = n
boolEval (NotExpr b) state = not (boolEval b state)
boolEval (BoolExpr Or l r) state = boolEval l state || boolEval r state
boolEval (BoolExpr And l r) state = boolEval l state && boolEval r state
boolEval (ArithExpr Eq l r) state = arithEval l state == arithEval r state
boolEval (ArithExpr Lt l r) state = arithEval l state < arithEval r state

-- Parse --

boolParse :: String -> BoolAST
boolParse s = let (fst, snd) = (last . readP_to_S boolExpr) s
    in if snd == "" then
        fst
    else
        Bool False

boolExpr :: ReadP BoolAST
boolExpr = foldr (\(op,name) p ->
    let this = p +++ do a <- p +++ parens boolExpr
                        char name
                        BoolExpr op a <$> this
            in this)
        ((parens arithBoolExpr <++ arithBoolExpr <++ bool) +++ (notExpr <++ parens boolExpr))
            boolOps

arithBoolExpr :: ReadP BoolAST
arithBoolExpr = do
    l <- arithExpr
    op <- char '=' <|> char '<'
    r <- arithExpr
    if op == '=' then
        return (ArithExpr Eq l r)
    else
        return (ArithExpr Lt l r)

notExpr :: ReadP BoolAST
notExpr = do
    skipSpaces
    char '¬'
    skipSpaces
    NotExpr <$> boolExpr

bool :: ReadP BoolAST
bool = do
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
boolASTToStr (ArithExpr Lt l r) = "(< " ++ arithASTToStr l ++ " " ++ arithASTToStr r ++ ")"

main :: IO ()
main = do
    expr <- getLine
    (print . ( `boolEval` empty) . boolParse) expr
