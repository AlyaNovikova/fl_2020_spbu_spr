module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative a reg = case reg of
    Empty   -> Empty
    Epsilon -> Empty
    Char c  -> if (a == c) then Epsilon else Empty
    Seq r s -> Alt (Seq (derivative a r) s) (toSet r (derivative a s))
    Alt r s -> Alt (derivative a r) (derivative a s)
    Star r  -> Seq (derivative a r) (Star r)

nullable :: Regexp -> Bool
nullable reg = case reg of
    Empty   -> False
    Epsilon -> True
    Char _  -> False
    Seq r s -> (nullable r) && (nullable s)
    Alt r s -> (nullable r) || (nullable s)
    Star _  -> True

-- конкатенируем вручную :))))
toSet :: Regexp -> Regexp -> Regexp
toSet r s | (nullable r) = s
          | otherwise = Empty
