module Test.LLang where

import           AST              (AST (..), Operator (..))
import qualified Data.Map         as Map
import           LLang
import           Combinators         (Parser (..), Result (..), runParser,
                                     symbol, symbols)
import           Control.Monad
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)


-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
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

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("X", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Write (Num 1))
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing



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
    runParser parseSeqNop "Nop   " @?= Success ""
        (Seq [])
    runParser parseSeqNop "Nop" @?= Success ""
        (Seq [])

    assertBool "" $ isFailure $ runParser parseSeqNop "Nop Write 0 Write 1"
    assertBool "" $ isFailure $ runParser parseSeqNop "Nop Read x Write x"
    assertBool "" $ isFailure $ runParser parseSeqNop "Nop vsem_privet"


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

    runParser parseL "Seq Nop Nop" @?= Success "" (Seq [Seq[], Seq[]])
    runParser parseL "Seq Nop Seq Nop Seq Nop Nop" @?= Success ""
     (Seq [Seq [], Seq [Seq[], Seq[Seq[], Seq[]]]])

    runParser parseL "Seq Seq Seq Read n If > n 0 Write n Nop Write * n -- 1 Write ! n" @?= Success ""
     (Seq
      [ Seq
        [ Seq
          [ Read "n",
            (If (BinOp Gt (Ident "n") (Num 0)) (Write (Ident "n")) (Seq []))
          ],
          Write (BinOp Mult (Ident "n") (UnaryOp Minus (Num 1)))
         ],
         (Write (UnaryOp Not (Ident "n")))
       ])

    runParser parseL "Write ! 1" @?= Success "" (Write (UnaryOp Not (Num 1)))
    runParser parseL "Write 1" @?= Success "" (Write (Num 1))
    runParser parseL "Read read" @?= Success "" (Read "read")
    runParser parseL "If ! x Nop Nop" @?= Success "" (If (UnaryOp Not (Ident "x")) (Seq []) (Seq []))
    runParser parseL "If -- x Nop Nop" @?= Success "" (If (UnaryOp Minus (Ident "x")) (Seq []) (Seq []))

    assertBool "" $ isFailure $ runParser parseL "Write Read"
    assertBool "" $ isFailure $ runParser parseL "If - x Nop Nop"
    assertBool "" $ isFailure $ runParser parseL "If  Nop Nop"
    assertBool "" $ isFailure $ runParser parseL "If  "
    assertBool "" $ isFailure $ runParser parseL "Seq Read a While x y"
    assertBool "" $ isFailure $ runParser parseL "shto proishodit?"
    assertBool "" $ isFailure $ runParser parseL "If a > 0 then a := -1 else a := 1"
    assertBool "" $ isFailure $ runParser parseL "Seq Nop"
