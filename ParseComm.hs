module ParseComm
( CommAST (..)
, comm
, commParse
) where

import Data.Map ( Map, empty, insert, toList )
import Text.ParserCombinators.ReadP
    ( ReadP, (+++), (<++), char, readP_to_S, skipSpaces, string )

import ParseArith ( ArithAST, arithExpr, varStr )
import ParseBool ( BoolAST, boolExpr )
import Gen ( braces, brackets )

data CommAST = SeqExpr {l :: CommAST, r :: CommAST}
             | WhileExpr { cond :: BoolAST, doComm :: CommAST}
             | IfExpr { cond :: BoolAST, ifComm :: CommAST, elseComm :: CommAST }
             | ArrAssignExpr { v :: String, i :: ArithAST, val :: ArithAST}
             | VarAssignExpr { v :: String, val :: ArithAST}
             | Skip

-- Parse --

commParse :: String -> CommAST
commParse s = let (fst, snd) = (last . readP_to_S comm) s
    in if snd == "" then
        fst
    else
        Skip

comm :: ReadP CommAST
comm = statement +++ braces comm +++ seqExpr

seqExpr :: ReadP CommAST
seqExpr = do
    skipSpaces
    c1 <- statement +++ braces comm
    skipSpaces
    char ';'
    skipSpaces
    c2 <- comm
    skipSpaces
    return (SeqExpr c1 c2)

statement :: ReadP CommAST
statement = skip +++ varAssignExpr +++ arrAssignExpr +++ ifExpr +++ whileExpr

varAssignExpr :: ReadP CommAST
varAssignExpr = do
    skipSpaces
    v <- varStr
    skipSpaces
    string ":="
    skipSpaces
    VarAssignExpr v <$> arithExpr

arrAssignExpr :: ReadP CommAST
arrAssignExpr = do
    skipSpaces
    v <- varStr
    skipSpaces
    i <- brackets arithExpr
    skipSpaces
    string ":="
    skipSpaces
    ArrAssignExpr v i <$> arithExpr

ifExpr :: ReadP CommAST
ifExpr = do
    skipSpaces
    string "if"
    skipSpaces
    b <- boolExpr
    skipSpaces
    string "then"
    skipSpaces
    ifComm <- statement <++ braces comm
    skipSpaces
    string "else"
    skipSpaces
    elseComm <- statement <++ braces comm
    skipSpaces
    return (IfExpr b ifComm elseComm)

whileExpr :: ReadP CommAST
whileExpr = do
    skipSpaces
    string "while"
    skipSpaces
    b <- boolExpr
    skipSpaces
    string "do"
    skipSpaces
    c <- statement <++ braces comm
    skipSpaces
    return (WhileExpr b c)

skip :: ReadP CommAST
skip = do
    skipSpaces
    string "skip"
    skipSpaces
    return Skip

instance Show CommAST where
    show Skip = "skip"
    show (VarAssignExpr s a) = s ++ " := " ++ show a
    show (ArrAssignExpr s i a) = s ++ "[" ++ show i ++ "]" ++ " := " ++ show a
    show (SeqExpr a b) = show a ++ "; " ++ show b
    show (IfExpr c a b) = "if " ++ show c ++ " then { " ++ show a ++ " } else { " ++ show b ++ " }"
    show (WhileExpr b c) = "while " ++ show b ++ " do { " ++ show c ++ " }"