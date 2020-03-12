module Combinators where

import           Control.Applicative
import           Control.Monad

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap f p = Parser $ \inp -> case (runParser p inp) of
      Success inp' res -> Success inp' (f res)
      Failure err -> Failure err


instance Applicative (Parser error input) where
  pure = return
  (<*>) = ap

instance Monad (Parser error input) where
  return res = Parser $ \inp -> Success inp res

  p >>= f = Parser $ \inp -> case (runParser p inp) of
      Success inp' res -> runParser (f res) inp'
      Failure err -> Failure err

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \inp -> Failure mempty

  p1 <|> p2 = Parser $ \inp -> case (runParser p1 inp) of
      Success inp' res -> Success inp' res
      Failure _ -> runParser p2 inp

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = do
    el <- elem
    elems <- many $ sep *> elem 
    return (el:elems)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: (Show a) => Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Show a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    []           -> Failure $ "Empty string"
    (x:xs)       -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success a = Parser $ \input -> Success input a

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure
