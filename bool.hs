module Bool 
    (BoolAST, boolEval, boolParse) where 

import Control.Applicative ( Alternative((<|>)) )
import Text.ParserCombinators.ReadP
    ( (+++), (<++), ReadP, char, string, many1, option, readP_to_S, satisfy, skipSpaces )

import Arith (ArithAST, number, arith, arithParse, arithEval, parens)

data BoolAST = BoolExpr {op :: BoolOps, l :: BoolAST, r :: BoolAST}
             | BoolNot {b :: BoolAST}
             | ArithExpr {aop :: ArithOps, al :: ArithAST, ar :: ArithAST }
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


boolEval :: BoolAST -> Bool
boolEval (Bool n) = n
boolEval (BoolNot b) = not (boolEval b)
boolEval (BoolExpr Or l r) = boolEval l || boolEval r
boolEval (BoolExpr And l r) = boolEval l && boolEval r

boolParse = fst . last . readP_to_S bool

bool = foldr (\(op,name) p ->
    let this = p +++ do a <- p +++ parens bool
                        char name
                        BoolExpr op a <$> this
            in this)
        (boolean +++ (boolNot <++ parens bool))
            boolOps

-- THIS DOESN'T MAKE SENSE BECAUSE I DO NOT RECURSIVELY APPLY arith OPS to bool
-- bool arith is on the same precedence as boolean primitive
boolArith = do
    l <- arith
    op <- ...
    r <- arith 
    return (ArithExpr op l r)

boolNot = do
    skipSpaces
    char '¬'
    BoolNot <$> bool


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
boolASTToStr (BoolNot b) = "(¬ " ++ boolASTToStr b ++ ")"
boolASTToStr (BoolExpr Or l r) = "(v " ++ boolASTToStr l ++ " " ++ boolASTToStr r ++ ")"
boolASTToStr (BoolExpr And l r) = "(^ " ++ boolASTToStr l ++ " " ++ boolASTToStr r ++ ")"

main :: IO ()
main = do
    expr <- getLine
    (print . boolEval . boolParse) expr
