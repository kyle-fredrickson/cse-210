import Data.Map ( Map, empty, insert, toList )
import Text.ParserCombinators.ReadP

import Arith ( ArithAST, arithEval, arithExpr, varStr )
import Bool ( BoolAST, boolEval, boolExpr )
import Gen ( braces, brackets )

data CommAST = SeqExpr {l :: CommAST, r :: CommAST}
             | WhileExpr { cond :: Bool.BoolAST, doComm :: CommAST}
             | IfExpr { cond :: Bool.BoolAST, ifComm :: CommAST, elseComm :: CommAST }
             | ArrAssignExpr { v :: String, i :: Arith.ArithAST, val :: Arith.ArithAST}
             | VarAssignExpr { v :: String, val :: Arith.ArithAST}
             | Skip deriving Show

-- Eval --

commEval :: CommAST -> Map String Integer
commEval n = let
    commEval' Skip store = store
    commEval' (VarAssignExpr v val) store = insert v (arithEval val store) store
    commEval' (ArrAssignExpr v i val) store = insert (v ++ show (arithEval i store)) (arithEval val store) store
    commEval' (IfExpr cond ifC elseC) store =
        if boolEval cond store then
            commEval' ifC store
        else
            commEval' elseC store
    commEval' (WhileExpr cond c) store =
        if boolEval cond store then
            commEval' (SeqExpr c (WhileExpr cond c)) store
        else
            store
    commEval' (SeqExpr c1 c2) store = commEval' c2 (commEval' c1 store)
    in commEval' n empty

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
    aExpr <- arithExpr
    return (VarAssignExpr v aExpr)

arrAssignExpr :: ReadP CommAST
arrAssignExpr = do
    skipSpaces
    v <- varStr
    skipSpaces
    i <- brackets arithExpr
    skipSpaces
    string ":="
    skipSpaces
    aExpr <- arithExpr
    return (ArrAssignExpr v i aExpr)

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

format :: Map String Integer -> String
format m = let
    format' [] = ""
    format' [(x,y)] = x ++ " → " ++ show y
    format' ((x,y):xs) = x ++ " → " ++ show y ++ ", " ++ format' xs
    in "{" ++ format' (toList m) ++ "}"

main :: IO ()
main = do
    expr <- getLine
    (putStr . format . commEval . commParse) expr
