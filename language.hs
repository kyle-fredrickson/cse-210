import Control.Applicative ( Alternative((<|>)) )
import Data.Char
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

parse :: [Char] -> AST
parse s = let l:r = parseToList s
    in parseOp (parseNum l) r

parseNum :: String -> AST
parseNum num = IntExpr (read num :: Integer)

parseOp :: AST -> [String] -> AST
parseOp left [] = left
parseOp left ("+":r) = parseAdd left r
parseOp left ("*":r) = parseMul left r
parseOp left ("^":r) = parseExp left r

parseAdd :: AST -> [String] -> AST
parseAdd left (num1:"*":num2:r) = AddExpr left (parseOp (MulExpr (parseNum num1) (parseNum num2)) r)
parseAdd left (num1:"^":num2:r) = AddExpr left (parseOp (ExpExpr (parseNum num1) (parseNum num2)) r)
parseAdd left (num:r) = parseOp (AddExpr left (parseNum num)) r

parseMul :: AST -> [String] -> AST
parseMul left (num1:"^":num2:r) = MulExpr left (parseOp (ExpExpr (parseNum num1) (parseNum num2)) r)
parseMul left (num:r) = parseOp (MulExpr left (parseNum num)) r

parseExp :: AST -> [String] -> AST
parseExp left (num:r) = parseOp (ExpExpr left (parseNum num)) r

-- Parser Parts --

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
    (print . eval . parse) expr