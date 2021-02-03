import Data.Map ( Map, empty, insert, toList )
import Text.ParserCombinators.ReadP

import ParseComm ( CommAST (..), commParse )
import EvalArith ( arithEval, Store, format )
import EvalBool ( boolEval )
import Gen ( braces, brackets )

-- Eval --

commEval :: CommAST -> Store
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

main :: IO ()
main = do
    expr <- getLine
    (putStr . format) (commEval (commParse expr))
