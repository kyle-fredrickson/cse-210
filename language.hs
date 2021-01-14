import Control.Applicative ( Alternative((<|>)) )
import Data.Char
import Data.List.Split
import System.Environment
import System.IO
import Text.ParserCombinators.ReadP

data Expr = IntExpr {n :: Integer}
          | SumExpr {l :: Expr, r :: Expr}
          | MulExpr {l :: Expr, r :: Expr}
          | ExpExpr {l :: Expr, r :: Expr}

eval :: Expr -> Integer
eval (IntExpr n) = n
eval (SumExpr l r) = eval l + eval r
eval (MulExpr l r) = eval l * eval r
eval (ExpExpr b e) = eval b ^ eval e

parse s =
    let l = parseToList s
    in parseNum l

parseNum [] = IntExpr 0
parseNum (x:[]) = IntExpr (read x :: Integer)
parseNum (x:rest) = parseOp (IntExpr (read x :: Integer)) rest

parseOp left [] = left
parseOp left ("+":rest) = SumExpr left (parseNum rest)
parseOp left ("*":num:rest) = parseOp (MulExpr left (parseNum [num])) rest
parseOp left ("^":num:rest) = parseOp (ExpExpr left (parseNum [num])) rest

-- Parser Parts --

parseToList :: [Char] -> [[Char]]
parseToList s =
    let parsed_string = readP_to_S arithExpr s
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
    string "*" <|>
    string "+" <|>
    string "^"

main :: IO ()
main = do
    expr <- getLine
    putStrLn "aaaaaahhh!"
    --(print . eval . parse . splitOneOf " *+()") (head args)