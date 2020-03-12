module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem',
                              fail', satisfy, success, symbol)
import           Data.Char   (digitToInt, isDigit)

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
        right = (do
            t <- term
            o <- op
            t' <- right
            return (f o t t')) <|> term

    helper (op, NoAssoc) term = do
        t <- term
        ((\o -> f o t) <$> op <*> term) <|> return t


plus  = symbol '+' >>= toOperator
minus = symbol '-' >>= toOperator
mult  = symbol '*' >>= toOperator
divv  = symbol '/' >>= toOperator
pow   = symbol '^' >>= toOperator

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [(plus <|> minus, LeftAssoc),
                      (mult <|> divv, LeftAssoc),
                      (pow, RightAssoc)]
                     (Num <$> parseNum <|> symbol '(' *> parseExpr <* symbol ')')
                     BinOp

-- Парсер для натуральных чисел с 0
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 `fmap` go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = success Plus
toOperator '*' = success Mult
toOperator '-' = success Minus
toOperator '/' = success Div
toOperator _   = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
