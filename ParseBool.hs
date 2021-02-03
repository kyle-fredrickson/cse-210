module ParseBool
( BoolAST (..)
, BoolOps (..)
, ArithOps (..)
, boolExpr
, boolParse
) where

import Control.Applicative ( Alternative((<|>)) )
import Text.ParserCombinators.ReadP
    ( (+++), (<++), char, readP_to_S, skipSpaces, string, ReadP )

import Gen ( parens )
import ParseArith ( ArithAST, arithExpr )

data BoolAST = BoolExpr {op :: BoolOps, l :: BoolAST, r :: BoolAST}
             | ArithExpr {aop :: ArithOps, al :: ArithAST, ar :: ArithAST }
             | NotExpr {b :: BoolAST}
             | Bool {n :: Bool}

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

instance Show BoolAST where
    show (Bool True) = "true"
    show (Bool False) = "false"
    show (NotExpr a) = "¬" ++ show a
    show (BoolExpr Or a b) = "(" ++ show a ++ "∨" ++ show b ++ ")"
    show (BoolExpr And a b) = "(" ++ show a ++ "∧" ++ show b ++ ")"
    show (ArithExpr Eq a b) = "(" ++ show a ++ "=" ++ show b ++ ")"
    show (ArithExpr Lt a b) = "(" ++ show a ++ "<" ++ show b ++ ")"