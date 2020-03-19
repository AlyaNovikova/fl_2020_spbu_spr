module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', elems',
                              fail', satisfy, success, symbol, symbols)
import           Data.Char   (digitToInt, isDigit, isLetter)

import           Control.Applicative
import           Control.Monad

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

-- Универсальный парсер выражений
uberExpr :: Monoid e
         => [(Parser e i op, Associativity)] -- список парсеров бинарных операторов с ассоциативностями в порядке повышения приоритета
         -> Parser e i ast -- парсер для элементарного выражения
         -> (op -> ast -> ast -> ast) -- функция для создания абстрактного синтаксического дерева для бинарного оператора
         -> Parser e i ast

uberExpr [] termP _ = termP

uberExpr opsP termP f = foldr helper termP opsP where
    helper (op, LeftAssoc) term = do
        t <- term
        foldl (\v1 (o, v2) -> f o v1 v2) t <$> many ((,) <$> op <*> term)

    helper (op, RightAssoc) term = right where
        right = do
            t <- term
            (do
                o <- op
                t' <- right
                return (f o t t')) <|> return t


    helper (op, NoAssoc) term = do
        t <- term
        ((\o -> f o t) <$> op <*> term) <|> return t




plus'      = symbols "+" >>= toOperator
minus'     = symbols "-" >>= toOperator
mult'      = symbols "*" >>= toOperator
pow'       = symbols "^" >>= toOperator
equal'     = symbols "==" >>= toOperator
nequal'    = symbols "/=" >>= toOperator
ge'        = symbols ">=" >>= toOperator
le'        = symbols "<=" >>= toOperator
and'       = symbols "&&" >>= toOperator
or'        = symbols "||" >>= toOperator
gt'        = symbols ">" >>= toOperator
lt'        = symbols "<" >>= toOperator
div'       = symbols "/" >>= toOperator

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [
                      (or', RightAssoc),
                      (and', RightAssoc),
                      (equal' <|> nequal' <|> ge' <|> le' <|> gt' <|> lt', NoAssoc),
                      (plus' <|> minus', LeftAssoc),
                      (mult' <|> div', LeftAssoc),
                      (pow', RightAssoc)
                     ]
                     (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
                     BinOp

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = Parser $ \input ->
    case input of
        ('-':xs) -> case runParser parseNum xs of
                    Success inp res -> Success inp (res * (-1))
                    er -> er
        otherwise -> runParser (foldl (\acc d -> 10 * acc + digitToInt d) 0 `fmap` go) input
                    where
                        go :: Parser String String String
                        go = some (satisfy isDigit)


parseIdent :: Parser String String String
parseIdent = do
    fs <- (satisfy isLetter) <|> (satisfy (== '_'))
    body <- many $ (satisfy isLetter) <|> (satisfy (== '_')) <|> (satisfy isDigit)
    return (fs:body)

operators' = ["+", "*", "-", "^", "==", "/=", ">=", "<=", ">", "<", "&&", "||", "/"]

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elems' operators' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = success Plus
toOperator "*"  = success Mult
toOperator "-"  = success Minus
toOperator "^"  = success Pow
toOperator "==" = success Equal
toOperator "/=" = success Nequal
toOperator ">=" = success Ge
toOperator "<=" = success Le
toOperator "&&" = success And
toOperator "||" = success Or
toOperator "/"  = success Div
toOperator "<"  = success Lt
toOperator ">"  = success Gt
toOperator _    = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

compute :: AST -> Int
compute (Num x)            = x
compute (BinOp Plus x y)   = compute x + compute y
compute (BinOp Mult x y)   = compute x * compute y
compute (BinOp Minus x y)  = compute x - compute y
compute (BinOp Div x y)    = compute x `div` compute y
compute (BinOp Pow x y)    = compute x ^ compute y
compute (BinOp Equal x y)  = boolToInt $ compute x == compute y
compute (BinOp Nequal x y) = boolToInt $ compute x /= compute y
compute (BinOp Ge x y)     = boolToInt $ compute x >= compute y
compute (BinOp Le x y)     = boolToInt $ compute x <= compute y
compute (BinOp Gt x y)     = boolToInt $ compute x > compute y
compute (BinOp Lt x y)     = boolToInt $ compute x < compute y
compute (BinOp And x y)    = case compute x of
                                0 -> 0
                                _ -> compute y
compute (BinOp Or x y)     = case compute x of
                                0 -> compute y
                                x -> x
