module LLang where

import           AST         (AST (..), Operator (..), Subst (..))
import           Combinators (Parser (..), Result (..), elem', elems',
                               satisfy, success, symbol, symbols, fail')
import           Data.Char   (isSpace)

import           Control.Applicative
import           Control.Monad
import           Expr (Associativity (..), parseNum, parseIdent, OpType (..), evalExpr)
import qualified Data.Map as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]



-- Эта функция нужна для преобразования
-- Parser er String res
-- в
-- Parser er [String] res
-- и наоборот

applyParser :: Alternative f => Parser e [i] a -> [i] -> f a
applyParser p x = case runParser p x of
    Success [] x -> pure x
    _            -> empty


parseNum' :: Parser String [String] Int
parseNum' = elem' >>= applyParser parseNum


parseOp'' :: Parser String String String
parseOp'' = (symbols "--") <|>
            (symbols "-") <|>
            (symbols ">=") <|>
            (symbols ">") <|>
            (symbols "<=") <|>
            (symbols "<") <|>
            (symbols "/=") <|>
            (symbols "==") <|>
            (symbols "&&") <|>
            (symbols "||") <|>
            (symbols "!") <|>
            (symbols "+") <|>
            (symbols "*") <|>
            (symbols "/") <|>
            (symbols "^")


parseOp' :: Parser String [String] String
parseOp' = elem' >>= applyParser parseOp''


keyWords = ["If", "While", "Assign", "Read", "Write", "Seq", "Nop"]

parseVar :: Parser String String String
parseVar = do
    var <- parseIdent
    guard (var `notElem` keyWords)
    return var


parseVar' :: Parser String [String] String
parseVar' = elem' >>= applyParser parseVar


-- Добавлен унарный минус
toOperator' :: String -> Parser String [String] Operator
toOperator' "+"  = success Plus
toOperator' "*"  = success Mult
toOperator' "--" = success Minus
toOperator' "-"  = success Minus
toOperator' "^"  = success Pow
toOperator' "==" = success Equal
toOperator' "/=" = success Nequal
toOperator' ">=" = success Ge
toOperator' "<=" = success Le
toOperator' "&&" = success And
toOperator' "||" = success Or
toOperator' "/"  = success Div
toOperator' "<"  = success Lt
toOperator' ">"  = success Gt
toOperator' "!"  = success Not
toOperator' _    = fail' "Failed toOperator'"


-- Обычный парсер выражений, но в польской записи
parseExpr' :: Parser String [String] AST
parseExpr' = (do
                opStr <- parseOp'
                if (opStr == "--" || opStr == "!")
                    then do
                        expr <- parseExpr'
                        op <- toOperator' opStr
                        return (UnaryOp op expr)
                    else do
                        expr1 <- parseExpr'
                        expr2 <- parseExpr'
                        op <- toOperator' opStr
                        return (BinOp op expr1 expr2)) <|>
             (do
                v <- parseVar'
                return (Ident v)) <|>
             (do
                 num <- parseNum'
                 return (Num num))



-- Запускаем парсер не на строке, а на списке токенов
parseExpr'' :: Parser String String AST
parseExpr'' = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseExpr' ts
    return res




-- Начинаются ключевые слова



parseStr :: String -> Parser String [String] String
parseStr str = (satisfy (== str))

parseAssign' :: Parser String [String] LAst
parseAssign' = do
    parseStr "Assign"
    v <- parseVar'
    expr <- parseExpr'
    return Assign {var = v, expr = expr}


parseRead' :: Parser String [String] LAst
parseRead' = do
    parseStr "Read"
    v <- parseVar'
    return Read {var = v}


parseWrite' :: Parser String [String] LAst
parseWrite' = do
    parseStr "Write"
    expr <- parseExpr'
    return Write {expr = expr}


parseWhile' :: Parser String [String] LAst
parseWhile' = do
    parseStr "While"
    expr <- parseExpr'
    ast <- parseL'
    return While { cond = expr, body = ast }


parseIf' :: Parser String [String] LAst
parseIf' = do
    parseStr "If"
    expr <- parseExpr'
    astL <- parseL'
    astR <- parseL'
    return If { cond = expr, thn = astL, els = astR }


parseSeq' :: Parser String [String] LAst
parseSeq' = do
    parseStr "Seq"
    ast1 <- parseL'
    ast2 <- parseL'
    return  Seq { statements = [ast1, ast2] }


-- Парсим пустую инстуркцию, Seq с пустым списком
parseSeqNop' :: Parser String [String] LAst
parseSeqNop' = do
    parseStr "Nop"
    return  Seq { statements = [] }




-- Парсит наш язык в виде списка токенов
parseL' :: Parser String [String] LAst
parseL' = parseIf' <|> parseWhile' <|> parseSeq' <|> parseAssign' <|> parseRead' <|> parseWrite' <|> parseSeqNop'

-- Запускаем parseL' на списке токенов
parseL :: Parser String String LAst
parseL = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseL' ts
    return res





-- ВСЕ следующие функции нужны только для тестирования (чтобы протестить каждое ключевое слово в отдельности)

parseAssign :: Parser String String LAst
parseAssign = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseAssign' ts
    return res

parseRead :: Parser String String LAst
parseRead = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseRead' ts
    return res


parseWrite :: Parser String String LAst
parseWrite = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseWrite' ts
    return res


parseWhile :: Parser String String LAst
parseWhile = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseWhile' ts
    return res


parseIf :: Parser String String LAst
parseIf = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseIf' ts
    return res


parseSeq :: Parser String String LAst
parseSeq = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseSeq' ts
    return res


parseSeqNop :: Parser String String LAst
parseSeqNop = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseSeqNop' ts
    return res






----------------------------------


initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval = \tree conf@(Conf subst inp outp) -> case tree of
    (Read var) -> case inp of
        (val:inp') -> Just $ Conf {subst = (Map.insert var val subst), input = inp', output = outp}
        otherwise -> Nothing

    (Write expr) -> do
        val <- evalExpr subst expr
        return $ Conf {subst = subst, input = inp, output = (val:outp)}

    (Assign var expr) -> do
        val <- evalExpr subst expr
        return $ Conf {subst = (Map.insert var val subst), input = inp, output = outp}

    (If expr last1 last2) -> do
        val <- evalExpr subst expr
        if (val /= 0)
            then eval last1 conf
            else eval last2 conf

    (While expr lAst) -> do
        val <- evalExpr subst expr
        if (val == 0)
            then return conf
            else do
                new_conf <- eval lAst conf
                eval (While expr lAst) new_conf

    (Seq []) -> Just conf
    (Seq (l:ls)) -> do
        new_conf <- eval l conf
        eval (Seq ls) new_conf
