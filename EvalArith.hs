module EvalArith
( arithEval
, arithParse
, Store
, format
) where

import Data.Map ( Map, lookup , empty, toList )
import Prelude hiding ( lookup )

import ParseArith ( ArithAST( ArithExpr, Var, Arr, Int ), ArithOps( Add, Sub, Mul, Exp ), arithParse )

type Store = Map String Integer

arithEval :: ArithAST -> Store -> Integer
arithEval (Int n) _ = n
arithEval (Var v) state = variableLookup (lookup v state)
arithEval (Arr v i) state = variableLookup (lookup (v ++ show (arithEval i state)) state)
arithEval (ArithExpr Add l r)  state = arithEval l  state + arithEval r  state
arithEval (ArithExpr Sub l r)  state = arithEval l  state - arithEval r  state
arithEval (ArithExpr Mul l r)  state = arithEval l  state * arithEval r  state
arithEval (ArithExpr Exp l r)  state = arithEval l  state ^ arithEval r  state

variableLookup :: Maybe Integer -> Integer
variableLookup (Just n) = n
variableLookup Nothing = 0

format :: Store -> String
format m = let
    format' [] = ""
    format' [(x,y)] = x ++ " → " ++ show y
    format' ((x,y):xs) = x ++ " → " ++ show y ++ ", " ++ format' xs
    in "{" ++ format' (toList m) ++ "}"