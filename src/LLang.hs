module LLang where

import           AST         (AST (..), Operator (..), Subst (..))

import           Data.List   (intercalate)
import qualified Data.Map    as Map
import           Text.Printf (printf)
import           Combinators (Parser (..), Result (..), InputStream (..), elem', elems',
                               satisfy, success, symbol, symbols, fail', runParser)
import           Data.Char   (isSpace)
import           Control.Applicative
import           Control.Monad
import           Expr (Associativity (..), parseNum, parseIdent, OpType (..), compute)



type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs }
                   deriving (Show, Eq)

type Defs = Map.Map String Function

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst, returnExpr :: Expr }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

-- Эта функция нужна для преобразования
-- Parser er String res
-- в
-- Parser er [String] res
-- и наоборот

applyParser :: Alternative f => Parser e [i] a -> [i] -> f a
applyParser p x = case runParser p x of
  Success (InputStream [] _) x -> pure x
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


keyWords = ["If", "While", "Assign", "Read", "Write", "Seq", "Nop",
            "Fun", "With", "ThatsAll", "Call", "Return", "WithFun", "ThatsAllFun"]


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



-- Парсер передаваемых аргументов функции при её вызове
parseArgCall' :: Parser String [String] [AST]
parseArgCall' = (do
                    parseStr "With"
                    x <- parseExpr'
                    xs <- parseArgCall'
                    return (x:xs)) <|>
                (do
                    parseStr "ThatsAll"
                    return [])


-- Парсер выражений в польской записи
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
               return (Num num)) <|>
            (do
                parseStr "Call"
                fun <- parseVar'
                vars <- parseArgCall'
                return (FunctionCall fun vars))



-- Запускаем парсер не на строке, а на списке токенов
parseExpr'' :: Parser String String AST
parseExpr'' = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseExpr' ts
    return res

parseArgCall :: Parser String String [AST]
parseArgCall = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseArgCall' ts
    return res


-- Начинаются основные ключевые слова



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





-- следующие функции нужны только для тестирования (чтобы протестить каждое ключевое слово в отдельности)

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



-- Парсер аргументов функции при её объявлении
parseArgFun' :: Parser String [String] [Var]
parseArgFun' = (do
                    parseStr "With"
                    x <- parseVar'
                    xs <- parseArgFun'
                    return (x:xs)) <|>
                (do
                    parseStr "ThatsAll"
                    return [])


parseDef' :: Parser String [String] Function
parseDef' = do
    parseStr "Fun"
    fun <- parseVar'
    vars <- parseArgFun'
    body <- parseL'
    parseStr "Return"
    rtrn <- parseExpr'
    return $ Function fun vars body rtrn



-- Парсер списка функций
parseFunList' :: Parser String [String] [Function]
parseFunList' = (do
                    parseStr "WithFun"
                    x <- parseDef'
                    xs <- parseFunList'
                    return (x:xs)) <|>
                (do
                    parseStr "ThatsAllFun"
                    return [])


parseProg' :: Parser String [String] Program
parseProg' = do
    funs <- parseFunList'
    main <- parseL'
    return $ Program funs main


-- запускаем наши парсеры на списке токенов
parseDef :: Parser String String Function
parseDef = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseDef' ts
    return res

parseProg :: Parser String String Program
parseProg = do
    ts <- words <$> many elem'
    Just res <- return $ applyParser parseProg' ts
    return res




-----------------






initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input [] Map.empty


evalFun :: Function -> [Int] -> Configuration -> Maybe (Configuration, Int)
evalFun (Function name args body rtrn)
        vars
        conf@(Conf subst inp outp def) =
            do
                subst' <- return $ Map.fromList (zip args vars)
                let conf0 = Conf subst' inp outp def
                conf1 <- eval body conf0
                (Conf _ inp' (ans:outp') _) <- eval (Write rtrn) conf1
                return (Conf subst inp' outp' def, ans)



evalExpr :: AST -> Configuration -> Maybe (Configuration, Int)
evalExpr = \tree conf@(Conf subst inp outp def) -> case tree of
    (Num x) -> Just (conf, x)

    (Ident v) -> do
        val <- Map.lookup v subst
        return (conf, val)

    (BinOp op ast1 ast2) -> do
        (conf1, val1) <- evalExpr ast1 conf
        (conf2, val2) <- evalExpr ast2 conf1
        return (conf2, compute $ BinOp op (Num val1) (Num val2))

    (UnaryOp op ast) -> do
        (conf', val) <- evalExpr ast conf
        return (conf', compute $ UnaryOp op (Num val))

    (FunctionCall str vars) ->
        let next vars' expr = do
                (conf0, xs) <- vars'
                (conf1, ans) <- evalExpr expr conf0
                return $ (conf1, xs ++ [ans])
        in do
            fun@(Function name args body rtrn) <- Map.lookup str def
            guard ((length vars) == (length args))

            (conf0@(Conf subst' _ _ _), expr) <- foldl next (Just (conf, [])) vars
            (conf1, ans) <- evalFun fun expr conf0
            return (Conf subst' (input conf1) (output conf1) (defs conf1), ans)


eval :: LAst -> Configuration -> Maybe Configuration
eval = \tree conf@(Conf subst inp outp def) -> case tree of
    (Read var) -> case inp of
        (val:inp') -> Just $ Conf {subst = (Map.insert var val subst), input = inp', output = outp, defs = def}
        otherwise -> Nothing

    (Write expr) -> do
        (Conf _ new_inp new_outp _, val) <- evalExpr expr conf
        return $ Conf {subst = subst, input = new_inp, output = (val:new_outp), defs = def}

    (Assign var expr) -> do
        (Conf _ new_inp new_outp _, val) <- evalExpr expr conf
        return $ Conf {subst = (Map.insert var val subst), input = new_inp, output = new_outp, defs = def}

    (If expr last1 last2) -> do
        (Conf _ new_inp new_outp _, val) <- evalExpr expr conf
        if (val /= 0)
            then eval last1 (Conf subst new_inp new_outp def)
            else eval last2 (Conf subst new_inp new_outp def)

    (While expr lAst) -> do
        (Conf _ new_inp new_outp _, val) <- evalExpr expr conf
        if (val == 0)
            then return (Conf subst new_inp new_outp def)
            else do
                new_conf <- eval lAst (Conf subst new_inp new_outp def)
                eval (While expr lAst) new_conf

    (Seq []) -> Just conf
    (Seq (l:ls)) -> do
        new_conf <- eval l conf
        eval (Seq ls) new_conf

----------------

instance Show Function where
  show (Function name args funBody returnExpr) =
    printf "%s(%s) =\n%s\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody) (identation 1 ("return " ++ show returnExpr))

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Eq Function where
    (==) f1 f2 = (show f1) == (show f2)

instance Eq Program where
    (==) p1 p2 = (show p1) == (show p2)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
