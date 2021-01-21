import Text.ParserCombinators.ReadP

import Arith ( ArithAST )
import Bool ( BoolAST, bool )

data CommAST = SeqExpr {l :: CommAST, r :: CommAST}
             | AssignExpr { v :: String, val :: Arith.ArithAST}
             | IfExpr { cond :: Bool.BoolAST, ifComm :: CommAST, elseComm :: CommAST }
             | WhileExpr { cond :: Bool.BoolAST, doComm :: CommAST}
             | Skip deriving Show

-- ifExpr = ...
whileExpr :: ReadP CommAST
whileExpr = do
    skipSpaces
    string "while"
    skipSpaces
    b <- bool
    skipSpaces
    string "do"
    skipSpaces
    -- c <- COMMANDS GO HERE
    -- skipSpaces
    return (WhileExpr b Skip)

skip :: ReadP CommAST
skip = do
    skipSpaces
    string "skip"
    skipSpaces
    return Skip