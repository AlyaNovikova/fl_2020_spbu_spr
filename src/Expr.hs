module Expr where

import           AST         (AST (..), Operator (..), Subst (..))
import           Combinators (Parser (..), Result (..), elem', elems',
                              fail', satisfy, success, symbol, symbols)
import           Data.Char   (digitToInt, isDigit, isLetter)

import           Control.Applicative
import           Control.Monad
import qualified Data.Map as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr = \subst ast -> case ast of
    (Num x) -> Just x
    (Ident v) -> Map.lookup v subst
    (UnaryOp op ast') -> do
        e <- evalExpr subst ast'
        return $ compute $ UnaryOp op (Num e)
    (BinOp op ast1 ast2) -> do
        e1 <- evalExpr subst ast1
        e2 <- evalExpr subst ast2
        return $ compute $ BinOp op (Num e1) (Num e2)


uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast

uberExpr [] termP _ _ = termP

uberExpr opsP termP pBin pUn = foldr helper termP opsP where

    helper (op, Unary) term = (pUn <$> op <*> term) <|> term

    helper (op, Binary NoAssoc) term = do
        t <- term
        ((\o -> pBin o t) <$> op <*> term) <|> return t

    helper (op, Binary LeftAssoc) term = do
        t <- term
        foldl (\v1 (o, v2) -> pBin o v1 v2) t <$> many ((,) <$> op <*> term)

    helper (op, Binary RightAssoc) term = right where
        right = do
            t <- term
            (do
                o <- op
                t' <- right
                return (pBin o t t')) <|> return t



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
not'       = symbols "!" >>= toOperator

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [
                      (or', Binary RightAssoc),
                      (and', Binary RightAssoc),
                      (not', Unary),
                      (equal' <|> nequal' <|> ge' <|> le' <|> gt' <|> lt', Binary NoAssoc),
                      (plus' <|> minus', Binary LeftAssoc),
                      (mult' <|> div', Binary LeftAssoc),
                      (minus', Unary),
                      (pow', Binary RightAssoc)
                     ]
                     (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
                     BinOp
                     UnaryOp

-- Парсер для положительных целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 `fmap` go
    where
        go :: Parser String String String
        go = some (satisfy isDigit)

-- Парсер для всех (и отрицательных тоже) целых чисел
parseNegNum :: Parser String String Int
parseNegNum = Parser $ \input ->
    case input of
        ('-':xs) -> runParser ((*(-1)) `fmap` parseNegNum) xs

        otherwise -> runParser (foldl (\acc d -> 10 * acc + digitToInt d) 0 `fmap` go) input
            where
                go :: Parser String String String
                go = some (satisfy isDigit)

parseIdent :: Parser String String String
parseIdent = do
    fs <- (satisfy isLetter) <|> (satisfy (== '_'))
    body <- many $ (satisfy isLetter) <|> (satisfy (== '_')) <|> (satisfy isDigit)
    return (fs:body)


operators' = ["+", "*", "-", "^", "==", "/=", ">=", "<=", ">", "<", "&&", "||", "/", "!"]

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
toOperator "!"  = success Not
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
compute (UnaryOp Minus x)  = -compute x
compute (UnaryOp Not x)    = boolToInt $ compute x == 0

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
