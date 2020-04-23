{-# LANGUAGE FlexibleInstances #-}

module Combinators where

import           Control.Applicative
import           Data.List           (nub, sortBy)
import           Control.Monad

data Result error input result
  = Success (InputStream input) result
  | Failure [ErrorMsg error]
  deriving (Eq)

data Position = Position { line :: Int, col :: Int }
                deriving (Show, Eq, Ord)

--класс чтобы реализовать функцию satisfy и elem'
class ForSatisfy a where
    incr :: a -> Position -> Position
    incr _ (Position l c) = Position l (c + 1)

instance ForSatisfy Int where
    incr _ (Position l c) = Position l (c + 1)

instance ForSatisfy Char where
    incr c (Position line col) = case c of
        '\n' -> Position (line + 1) 0
        '\t' -> Position line (col + 4)
        ' ' -> Position line (col + 1)
        otherwise -> Position line (col + 1)

instance ForSatisfy String where
    incr s (Position l c) = Position l (c + length s)



newtype Parser error input result
  = Parser { runParser' :: (InputStream input) -> Result error input result }

data InputStream a = InputStream { stream :: a, curPos :: Position }
                   deriving (Show, Eq)

data ErrorMsg e = ErrorMsg { errors :: [e], pos :: Position }
                deriving (Eq)

makeError e p = ErrorMsg [e] p

initPosition = Position 0 0

runParser :: Parser error input result -> input -> Result error input result
runParser parser input = runParser' parser (InputStream input initPosition)

toStream :: a -> Position -> InputStream a
toStream = InputStream

incrPos :: InputStream a -> InputStream a
incrPos (InputStream str (Position l c)) = InputStream str (Position l (c + 1))
-- incrPos :: Char -> Position -> Position
-- incrPos c (Position line col) = case c of
--     '\n' -> Position (line + 1) 0
--     '\t' -> Position line (col + 4)
--     ' ' -> Position line (col + 1)
--     otherwise -> Position line (col + 1)

instance Functor (Parser error input) where
  fmap f (Parser p) = Parser $ \inp -> case (p inp) of
      Success inp' res -> Success inp' (f res)
      Failure err -> Failure err


instance Applicative (Parser error input) where
  pure = return
  (<*>) = ap

instance Monad (Parser error input) where
  return res = Parser $ \inp -> Success inp res

  (Parser p) >>= f = Parser $ \inp -> case (p inp) of
      Success inp' res -> runParser' (f res) inp'
      Failure err -> Failure err

instance Monoid error => MonadFail (Parser error input) where
    fail _ = empty

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure [makeError mempty (curPos input)]

  Parser a <|> Parser b = Parser $ \input ->
    case a input of
      Success input' r -> Success input' r
      Failure e ->
        case b input of
          Failure e' -> Failure $ mergeErrors e e'
          x          -> x

mergeErrors :: (Monoid e) => [ErrorMsg e] -> [ErrorMsg e] -> [ErrorMsg e]
mergeErrors e e' =
    merge (sortBy sorting e) (sortBy sorting e')
  where
    merge [] s = s
    merge s [] = s
    merge (ErrorMsg e p : xs) (ErrorMsg e' p' : xs') | p == p' = ErrorMsg (e <> e') p : merge xs xs'
    merge (ErrorMsg e p : xs) e'@(ErrorMsg _ p' : _) | p < p' = ErrorMsg e p : merge xs e'
    merge e@(ErrorMsg _ p : _) (ErrorMsg e' p' : xs) | p > p' = ErrorMsg e' p' : merge xs e

    sorting x y = pos x `compare` pos y

infixl 1 <?>
(<?>) :: Monoid error => error -> Parser error input a -> Parser error input a
(<?>) msg (Parser p) = Parser $ \input ->
    case p input of
      Failure err -> Failure $ mergeErrors [makeError msg (maximum $ map pos err)] err
      x -> x

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = ("Expected symbol: " ++ show c) <?> satisfy (== c)

--Проверяет, что первые несколько элементов входной последовательности -- данные символы
symbols :: String -> Parser String String String
symbols (x:xs) = do
    a <- symbol x
    as <- symbols xs
    return (a:as)
symbols [] = Parser $ \input -> Success input ""

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: (ForSatisfy a) => Parser String [a] a
elem' = satisfy (const True)

elems' :: [String] -> Parser String String String
elems' (x:xs) = symbols x <|> elems' xs
elems' [] = Parser $ \input -> Success input ""

eof :: Parser String String ()
eof = Parser $ \input -> if null $ stream input then Success input () else Failure [makeError "Not eof" (curPos input)]

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (ForSatisfy a) => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \(InputStream input pos) ->
  case input of
    (x:xs) | p x -> Success (InputStream xs (incr x pos)) x
    input        -> Failure [makeError "Predicate failed" pos]

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success a = Parser $ \input -> Success input a

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' msg = Parser $ \input -> Failure [makeError msg (curPos input)]

word :: String -> Parser String String String
word w = Parser $ \(InputStream input (Position l c)) ->
  let (pref, suff) = splitAt (length w) input in
  if pref == w
  then Success (InputStream suff (Position l (c + length w))) w
  else Failure [makeError ("Expected " ++ show w) (Position l c)]

instance Show (ErrorMsg String) where
  show (ErrorMsg e pos) = "at position " ++ show pos ++ ":\n" ++ (unlines $ map ('\t':) (nub e))

instance (Show input, Show result) => Show (Result String input result) where
  show (Failure e) = "Parsing failed\n" ++ unlines (map show e)
  show (Success i r) = "Parsing succeeded!\nResult:\n" ++ show r ++ "\nSuffix:\t" ++ show i
