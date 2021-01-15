import Control.Applicative ( Alternative((<|>)) )
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

data AST = IntExpr {n :: Integer}
         | AddExpr {l :: AST, r :: AST}
         | MulExpr {l :: AST, r :: AST}
         | ExpExpr {l :: AST, r :: AST}

-- Eval --

eval :: AST -> Integer
eval (IntExpr n) = n
eval (AddExpr l r) = eval l + eval r
eval (MulExpr l r) = eval l * eval r
eval (ExpExpr b e) = eval b ^ eval e

-- Parse to AST --

precedence :: [String]
precedence = ["+", "*", "^"]

parse :: [String] -> AST
parse [num] = IntExpr (read num :: Integer)
parse xs = reconstruct (splitOnLowestPrecedence xs)

reconstruct (l, "+", r) = AddExpr (parse l) (parse r)
reconstruct (l, "*", r) = MulExpr (parse l) (parse r)
reconstruct (l, "^", r) = ExpExpr (parse l) (parse r)

splitOnLowestPrecedence :: [String] -> ([String], String, [String])
splitOnLowestPrecedence s = let idx = opIndices s
    in (take idx s, s !! idx, drop (idx + 1) s)

opIndices :: [String] -> Int
opIndices s = let (Just (Just n)) = find isJust (map (\ x -> (findIndex (x ==) s)) precedence)
    in n

parseToList :: [Char] -> [String]
parseToList s = let parsed_string = readP_to_S arithExpr s
    in if null parsed_string || snd (last parsed_string) /= ""
       then []
       else let ((num1, rest), _) = last parsed_string
            in let l = num1:foldr (\ (x, y) z -> x:y:z) [] rest
                in l

arithExpr :: ReadP ([Char], [([Char], [Char])])
arithExpr = do
    num <- number
    rest <- many expr
    return (num, rest)

expr :: ReadP ([Char], [Char])
expr = do
    op <- arithOp
    skipMany (char ' ')
    num2 <- number
    skipMany (char ' ')
    return (op, num2)

number :: ReadP [Char]
number = do
    minus <- option '0' (char '-')
    digits <- many1 (satisfy isDigit)
    skipMany (char ' ')
    return (minus : digits)

arithOp :: ReadP [Char]
arithOp =
    string "+" <|>
    string "*" <|>
    string "^"

-- Debugging Tools --

exprToStr :: AST -> [Char]
exprToStr (IntExpr n) = show n
exprToStr (AddExpr l r) = "(+ " ++ exprToStr l ++ " " ++ exprToStr r ++ ")"
exprToStr (MulExpr l r) = "(* " ++ exprToStr l ++ " " ++ exprToStr r ++ ")"
exprToStr (ExpExpr l r) = "(^ " ++ exprToStr l ++ " " ++ exprToStr r ++ ")"

main :: IO ()
main = do
    expr <- getLine
    (print . eval . parse . parseToList) expr