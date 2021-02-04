import Data.Map ( Map, empty, insert, toList )
import Text.ParserCombinators.ReadP

import ParseComm ( CommAST (..), commParse )
import EvalArith ( arithEval, Store, formatStore )
import EvalBool ( boolEval )
import Gen ( braces, brackets )

-- Eval --

commEval :: CommAST -> Store -> (CommAST, Store)
commEval Skip store = (Skip, store)
commEval (VarAssignExpr v val) store = (Skip, insert v (arithEval val store) store)
-- commEval' (ArrAssignExpr v i val) store = .
commEval (IfExpr cond ifC elseC) store =
    if boolEval cond store then
        (ifC, store)
    else
        (elseC, store)
commEval (WhileExpr cond c) store =
    if boolEval cond store then
        (SeqExpr c (WhileExpr cond c), store)
    else
        (Skip, store)
commEval (SeqExpr Skip c2) store = (c2, store)
commEval (SeqExpr c1 c2) store = let (c1', store') = commEval c1 store
    in (SeqExpr c1' c2, store')

run :: (CommAST, Store) -> Integer -> String
run (Skip, empty) _ = ""
run _ 10000 = ""
run (p, s) n = let (next, store) = commEval p s
    in "⇒ " ++ show next ++ ", " ++ formatStore store ++ "\n" ++ run (next, store) (n + 1)

main :: IO ()
main = do
    expr <- getLine
    putStr (run (commParse expr, empty) 0)
