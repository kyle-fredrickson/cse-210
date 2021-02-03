module EvalBool
( boolEval
, boolParse
) where

import Data.Map ( Map , empty)

import Gen ( parens )
import ParseArith ( ArithAST, arithExpr )
import EvalArith ( arithEval, Store )
import ParseBool ( BoolAST ( BoolExpr, ArithExpr, NotExpr, Bool), BoolOps ( Or, And ), ArithOps ( Eq, Lt), boolParse )

boolEval :: BoolAST -> Store -> Bool
boolEval (Bool n) _ = n
boolEval (NotExpr b) state = not (boolEval b state)
boolEval (BoolExpr Or l r) state = boolEval l state || boolEval r state
boolEval (BoolExpr And l r) state = boolEval l state && boolEval r state
boolEval (ArithExpr Eq l r) state = arithEval l state == arithEval r state
boolEval (ArithExpr Lt l r) state = arithEval l state < arithEval r state