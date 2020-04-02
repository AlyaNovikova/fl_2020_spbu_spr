module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol, symbols)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), evaluate, parseExpr,
                                      parseNum, parseNegNum, parseOp, toOperator, uberExpr, parseIdent, OpType (..))
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)
import           LLang


isFailure (Failure _) = True
isFailure  _          = False

unit_parseVar :: Assertion
unit_parseVar = do
    runParser parseVar "abc def" @?= Success " def" "abc"
    runParser parseVar "AbC dEf" @?= Success " dEf" "AbC"
    runParser parseVar "_123" @?= Success "" "_123"
    runParser parseVar "a_b_c d_e" @?= Success " d_e" "a_b_c"
    runParser parseVar "x_ " @?= Success " " "x_"
    runParser parseVar "abc123" @?= Success "" "abc123"
    runParser parseVar "_" @?= Success "" "_"
    runParser parseVar "abc*1" @?= Success "*1" "abc"
    runParser parseVar "If1" @?= Success "" "If1"
    runParser parseVar "_While" @?= Success "" "_While"
    assertBool "" $ isFailure $ runParser parseVar "123abc"
    assertBool "" $ isFailure $ runParser parseVar "123"
    assertBool "" $ isFailure $ runParser parseVar "If"
    assertBool "" $ isFailure $ runParser parseVar "While"
    assertBool "" $ isFailure $ runParser parseVar "Assign"
    assertBool "" $ isFailure $ runParser parseVar "Read"
    assertBool "" $ isFailure $ runParser parseVar "Write"
    assertBool "" $ isFailure $ runParser parseVar "Seq"


unit_parseExpr' :: Assertion
unit_parseExpr' = do
    runParser parseExpr'' "+    --   1 \n  2" @?= Success "" (BinOp Plus (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr'' "* 2 x" @?= Success "" (BinOp Mult (Num 2) (Ident "x"))
    runParser parseExpr'' "* 2 * x 3" @?= Success "" (BinOp Mult (Num 2) (BinOp Mult (Ident "x") (Num 3)))
    runParser parseExpr'' "* * 2 x 3" @?= Success "" (BinOp Mult (BinOp Mult (Num 2) (Ident "x")) (Num 3))

    assertBool "" $ isFailure $ runParser parseExpr'' "++ 1 2"
    assertBool "" $ isFailure $ runParser parseExpr'' "x -- 2"
    assertBool "" $ isFailure $ runParser parseExpr'' "1 + 2"
    assertBool "" $ isFailure $ runParser parseExpr'' "!= 2 1"
    assertBool "" $ isFailure $ runParser parseExpr'' "  - 2 "


unit_parseAssign :: Assertion
unit_parseAssign = do
    runParser parseAssign "Assign ___\n0" @?= Success "" (Assign {var = "___", expr = (Num 0)})
    runParser parseAssign "Assign   \t  Alya    + x y" @?= Success "" (Assign {var = "Alya", expr =
        (BinOp Plus (Ident "x") (Ident "y"))})
    runParser parseAssign "Assign  _kek  -- y" @?= Success "" (Assign {var = "_kek", expr =
        (UnaryOp Minus (Ident "y"))})


    assertBool "" $ isFailure $ runParser parseAssign "Assign x1"
    assertBool "" $ isFailure $ runParser parseAssign "Assign jhabljhb"
    assertBool "" $ isFailure $ runParser parseAssign "Assignx 1"
    assertBool "" $ isFailure $ runParser parseAssign "Assign 1 2"


unit_parseRead :: Assertion
unit_parseRead = do
    runParser parseRead "Read x" @?= Success "" (Read {var = "x"})
    runParser parseRead "Read _" @?= Success "" (Read {var = "_"})

    assertBool "" $ isFailure $ runParser parseRead "Read 1"
    assertBool "" $ isFailure $ runParser parseRead "Read + 1 2"
    assertBool "" $ isFailure $ runParser parseRead "Read x + 1 2"


unit_parseWrite :: Assertion
unit_parseWrite = do
    runParser parseWrite "Write x " @?= Success "" (Write {expr = (Ident "x")})
    runParser parseWrite "Write 1 " @?= Success "" (Write {expr = (Num 1)})
    runParser parseWrite "Write ^ ! x 3" @?= Success "" (Write {expr =
        (BinOp Pow (UnaryOp Not (Ident "x")) (Num 3)) })

    assertBool "" $ isFailure $ runParser parseWrite "Write 1kek"
    assertBool "" $ isFailure $ runParser parseWrite "Write ! "
    assertBool "" $ isFailure $ runParser parseWrite "Write + 1 "


unit_parseIf :: Assertion
unit_parseIf = do
    runParser parseIf "If xx Write 1 Write 0 " @?= Success ""
        (If {cond = (Ident "xx"), thn = (Write {expr = (Num 1)}), els = (Write {expr = (Num 0)}) })
    runParser parseIf "If + 1 2 Read x Write 0" @?= Success ""
        (If {cond = (BinOp Plus (Num 1) (Num 2)), thn = (Read {var = "x"}), els = (Write {expr = (Num 0)}) })

    assertBool "" $ isFailure $ runParser parseIf "If x Write 1 0 "
    assertBool "" $ isFailure $ runParser parseIf "If x Write 1 Write 0AA"
    assertBool "" $ isFailure $ runParser parseIf "If + 1 2 Read 1 Write 0 "


unit_parseWhile :: Assertion
unit_parseWhile = do
    runParser parseWhile "While xx Assign kek 0" @?= Success ""
        (While {cond = (Ident "xx"), body = (Assign {var = "kek", expr = (Num 0)}) })
    runParser parseWhile "While > - x 5 0 Write x" @?= Success ""
        (While {cond = (BinOp Gt (BinOp Minus (Ident "x") (Num 5)) (Num 0)), body = (Write {expr = (Ident "x")}) })

    assertBool "" $ isFailure $ runParser parseWhile "If x Write 1 Write 0"
    assertBool "" $ isFailure $ runParser parseWhile "while kek mem"
    assertBool "" $ isFailure $ runParser parseWhile "Privet mir!"



unit_parseSeq :: Assertion
unit_parseSeq = do
    runParser parseSeq "Seq Read x Write x" @?= Success ""
        (Seq [(Read {var = "x"}), (Write {expr = (Ident "x")})])

    assertBool "" $ isFailure $ runParser parseSeq "Seq Write 0 Write"
    assertBool "" $ isFailure $ runParser parseSeq "Seq"
    assertBool "" $ isFailure $ runParser parseSeq "Seq vsem_privet"


unit_parseSeqNop :: Assertion
unit_parseSeqNop = do
    runParser parseSeqNop "Seq   " @?= Success ""
        (Seq [])
    runParser parseSeqNop "Seq" @?= Success ""
        (Seq [])

    assertBool "" $ isFailure $ runParser parseSeqNop "Seq Write 0 Write 1"
    assertBool "" $ isFailure $ runParser parseSeqNop "Seq Read x Write x"
    assertBool "" $ isFailure $ runParser parseSeqNop "Seq vsem_privet"


unit_parseL :: Assertion
unit_parseL = do
    runParser parseL "If xx Write 1 Write 0 " @?= Success ""
        (If {cond = (Ident "xx"), thn = (Write {expr = (Num 1)}), els = (Write {expr = (Num 0)}) })
    runParser parseL "Seq Read X If > X 13 Write X While < X 42 Seq Assign X * X 7 Write X" @?= Success ""
        (Seq
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
          ])

    -- факториал (проверка на простоту оказалась сильнее меня)
    runParser parseL "Seq Seq Seq Read n Assign cur 1 While > n 0 Seq Assign cur * cur n Assign n - n 1 Write n" @?= Success ""
        (Seq
         [ Seq
           [ Seq
             [ Read "n",
               Assign "cur" (Num 1)
             ],
             (While (BinOp Gt (Ident "n") (Num 0))
                    (Seq
                     [ Assign "cur" (BinOp Mult (Ident "cur") (Ident "n")),
                       Assign "n"   (BinOp Minus (Ident "n") (Num 1))
                     ]))
            ],
            (Write (Ident "n"))
          ])

    runParser parseL "Seq Seq Seq" @?= Success "" (Seq [Seq[], Seq[]])

    assertBool "" $ isFailure $ runParser parseL "   "
    assertBool "" $ isFailure $ runParser parseL "If  "
    assertBool "" $ isFailure $ runParser parseL "Seq Read a While x y"
    assertBool "" $ isFailure $ runParser parseL "shto proishodit?"
    assertBool "" $ isFailure $ runParser parseL "If a > 0 then a := -1 else a := 1"
    assertBool "" $ isFailure $ runParser parseL "Seq Seq"
