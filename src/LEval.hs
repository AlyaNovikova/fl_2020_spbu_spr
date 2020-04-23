module LEval where

import qualified Data.Map    as Map
import Combinators (InputStream (..), Result (..), runParser)
import LLang (eval, Program (..), Configuration (..), Function (..), parseProg)

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program funs main) inp = eval main
    (Conf Map.empty inp [] (Map.fromList (fmap (\fun@(Function name _ _ _) -> (name, fun)) funs)))

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg str inp = case runParser parseProg str of
    Success (InputStream _ _) ans -> evalProg ans inp
    otherwise -> Nothing
