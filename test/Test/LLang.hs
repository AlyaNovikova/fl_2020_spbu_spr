module Test.LLang where

import           AST
import           Combinators
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)
import           Control.Monad

-- -- f x y = read z ; return (x + z * y)
-- -- g x = if (x) then return x else return x*13
-- -- {read x; read y; write (f x y); write (g x)}"
--
-- prog =
--   Program
--     [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
--     , Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))
--     ]
--     (
--       Seq
--         [ Read "x"
--         , Read "y"
--         , Write (FunctionCall "f" [Ident "x", Ident "y"])
--         , Write (FunctionCall "g" [Ident "x"])
--         ]
--     )
--
-- -- read x;
-- -- if (x > 13)
-- -- then { write x }
-- -- else {
-- --     while (x < 42) {
-- --       x := x * 7;
-- --       write (x);
-- --     }
-- -- }
-- stmt1 :: LAst
-- stmt1 =
--   Seq
--     [ Read "x"
--     , If (BinOp Gt (Ident "x") (Num 13))
--          (Seq [(Write (Ident "x"))])
--          (Seq [(While (BinOp Lt (Ident "x") (Num 42))
--                 (Seq [ Assign "x"
--                         (BinOp Mult (Ident "x") (Num 7))
--                      , Write (Ident "x")
--                      ]
--                 )
--          )])
--     ]
--
-- unit_stmt1 :: Assertion
-- unit_stmt1 = do
--   let xIs n = Map.fromList [("x", n)]
--   eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
--   eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
--   eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])
--
--
-- -- read x;
-- -- if (x)
-- -- then {
-- --   while (x) {
-- --     x := x - 2;
-- --     write (x);
-- --   }
-- -- else {}
-- stmt2 :: LAst
-- stmt2 =
--   Seq
--     [ Read "x"
--     , If (Ident "x")
--          (Seq [(While (Ident "x")
--                 (Seq
--                    [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
--                    , (Write (Ident "x"))
--                    ]
--                 )
--          )])
--          (Seq [])
--     ]
--
-- unit_stmt2 :: Assertion
-- unit_stmt2 = do
--   let xIs n = Map.fromList [("x", n)]
--   eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
--   eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
--   eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))
--
-- -- read x;
-- -- read y;
-- -- write (x == y);
-- stmt3 :: LAst
-- stmt3 =
--   Seq
--     [ Read "x"
--     , Read "y"
--     , Write (BinOp Equal (Ident "x") ((Ident "y")))
--     ]
--
-- unit_stmt3 :: Assertion
-- unit_stmt3 = do
--   let subst x y = Map.fromList [("x", x), ("y", y) ]
--   eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
--   eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
--   eval stmt3 (initialConf [42]) @?= Nothing

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
-- stmt4 :: LAst
-- stmt4 =
--   Seq
--     [ Read "n"
--     , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
--          (Seq [(Write (Num 1))])
--          (Seq
--             [ Assign "i" (Num 2)
--             , Assign "cur" (Num 1)
--             , Assign "prev" (Num 1)
--             , While (BinOp Lt (Ident "i") (Ident "n"))
--                      (Seq
--                         [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
--                         , Assign "prev" (Ident "cur")
--                         , Assign "cur" (Ident "temp")
--                         , Assign "i" (BinOp Plus (Ident "i") (Num 1))
--                         ]
--                      )
--             , Write (Ident "cur")
--             ]
--          )
--     ]
--
-- unit_stmt4 :: Assertion
-- unit_stmt4 = do
--   let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
--   let subst' n = Map.fromList [("n", n)]
--   eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
--   eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
--   eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
--   eval stmt4 (initialConf []) @?= Nothing


isFailure (Failure _) = True
isFailure  _          = False

unit_parseVar :: Assertion
unit_parseVar = do
  runParser parseVar "abc def" @?= Success (toStream " def" (Position 0 3)) "abc"
  runParser parseVar "AbC dEf" @?= Success (toStream " dEf" (Position 0 3)) "AbC"
  runParser parseVar "_123" @?= Success (toStream "" (Position 0 4)) "_123"
  runParser parseVar "a_b_c d_e" @?= Success (toStream " d_e" (Position 0 5)) "a_b_c"
  runParser parseVar "x_ " @?= Success (toStream " " (Position 0 2)) "x_"
  runParser parseVar "abc123" @?= Success (toStream "" (Position 0 6)) "abc123"
  runParser parseVar "_" @?= Success (toStream "" (Position 0 1)) "_"
  runParser parseVar "abc*1" @?= Success (toStream "*1" (Position 0 3)) "abc"
  runParser parseVar "If1" @?= Success (toStream "" (Position 0 3)) "If1"
  runParser parseVar "_While" @?= Success (toStream "" (Position 0 6)) "_While"
  assertBool "" $ isFailure $ runParser parseVar "123abc"
  assertBool "" $ isFailure $ runParser parseVar "123"
  assertBool "" $ isFailure $ runParser parseVar "If"
  assertBool "" $ isFailure $ runParser parseVar "While"
  assertBool "" $ isFailure $ runParser parseVar "Assign"
  assertBool "" $ isFailure $ runParser parseVar "Read"
  assertBool "" $ isFailure $ runParser parseVar "Write"
  assertBool "" $ isFailure $ runParser parseVar "Seq"
  assertBool "" $ isFailure $ runParser parseVar "With"
  assertBool "" $ isFailure $ runParser parseVar "ThatsAll"
  assertBool "" $ isFailure $ runParser parseVar "Call"
  assertBool "" $ isFailure $ runParser parseVar "Fun"





runParser2 :: (Eq a, Show a) => Parser String String a -> String -> a -> Assertion
runParser2 pars prog ans = do
  case runParser pars prog of
       Success (InputStream "" _) _ -> (True @?= True)
       otherwise -> (False @?= True)



unit_parseExpr' :: Assertion
unit_parseExpr' = do
  runParser2 parseExpr'' "+    --   1 \n  2" (BinOp Plus (UnaryOp Minus (Num 1)) (Num 2))
  runParser2 parseExpr'' "* 2 x" (BinOp Mult (Num 2) (Ident "x"))
  runParser2 parseExpr'' "* 2 * x 3" (BinOp Mult (Num 2) (BinOp Mult (Ident "x") (Num 3)))
  runParser2 parseExpr'' "* * 2 x 3" (BinOp Mult (BinOp Mult (Num 2) (Ident "x")) (Num 3))
  runParser2 parseExpr'' "Call kek With * x 3 ThatsAll" (FunctionCall "kek" [BinOp Mult (Ident "x") (Num 3)])
  runParser2 parseExpr'' "Call Alya ThatsAll" (FunctionCall "Alya" [])
  runParser2 parseExpr'' "Call theBestFunction With a With b With c ThatsAll"
    (FunctionCall "theBestFunction" [(Ident "a"), (Ident "b"), (Ident "c")])
  runParser2 parseExpr'' "Call f With 1 ThatsAll"
    (FunctionCall "f" [(Num 1)])

  assertBool "" $ isFailure $ runParser parseExpr'' "++ 1 2"
  assertBool "" $ isFailure $ runParser parseExpr'' "x -- 2"
  assertBool "" $ isFailure $ runParser parseExpr'' "1 + 2"
  assertBool "" $ isFailure $ runParser parseExpr'' "!= 2 1"
  assertBool "" $ isFailure $ runParser parseExpr'' "  - 2 "
  assertBool "" $ isFailure $ runParser parseExpr'' "Call kek With * x 3 + 1 2"
  assertBool "" $ isFailure $ runParser parseExpr'' "Call kek With ThatsAll"


unit_parseAssign :: Assertion
unit_parseAssign = do
  runParser2 parseAssign "Assign ___\n0" (Assign {var = "___", expr = (Num 0)})
  runParser2 parseAssign "Assign   \t  Alya    + x y" (Assign {var = "Alya", expr =
   (BinOp Plus (Ident "x") (Ident "y"))})
  runParser2 parseAssign "Assign  _kek  -- y" (Assign {var = "_kek", expr =
   (UnaryOp Minus (Ident "y"))})


  assertBool "" $ isFailure $ runParser parseAssign "Assign x1"
  assertBool "" $ isFailure $ runParser parseAssign "Assign jhabljhb"
  assertBool "" $ isFailure $ runParser parseAssign "Assignx 1"
  assertBool "" $ isFailure $ runParser parseAssign "Assign 1 2"


unit_parseRead :: Assertion
unit_parseRead = do
  runParser2 parseRead "Read x" (Read {var = "x"})
  runParser2 parseRead "Read _" (Read {var = "_"})

  assertBool "" $ isFailure $ runParser parseRead "Read 1"
  assertBool "" $ isFailure $ runParser parseRead "Read + 1 2"
  assertBool "" $ isFailure $ runParser parseRead "Read x + 1 2"


unit_parseWrite :: Assertion
unit_parseWrite = do
  runParser2 parseWrite "Write x " (Write {expr = (Ident "x")})
  runParser2 parseWrite "Write 1 " (Write {expr = (Num 1)})
  runParser2 parseWrite "Write ^ ! x 3" (Write {expr =
   (BinOp Pow (UnaryOp Not (Ident "x")) (Num 3)) })

  assertBool "" $ isFailure $ runParser parseWrite "Write 1kek"
  assertBool "" $ isFailure $ runParser parseWrite "Write ! "
  assertBool "" $ isFailure $ runParser parseWrite "Write + 1 "


unit_parseIf :: Assertion
unit_parseIf = do
  runParser2 parseIf "If xx Write 1 Write 0 "
   (If {cond = (Ident "xx"), thn = (Write {expr = (Num 1)}), els = (Write {expr = (Num 0)}) })
  runParser2 parseIf "If + 1 2 Read x Write 0"
   (If {cond = (BinOp Plus (Num 1) (Num 2)), thn = (Read {var = "x"}), els = (Write {expr = (Num 0)}) })

  assertBool "" $ isFailure $ runParser parseIf "If x Write 1 0 "
  assertBool "" $ isFailure $ runParser parseIf "If x Write 1 Write 0AA"
  assertBool "" $ isFailure $ runParser parseIf "If + 1 2 Read 1 Write 0 "


unit_parseWhile :: Assertion
unit_parseWhile = do
  runParser2 parseWhile "While xx Assign kek 0"
   (While {cond = (Ident "xx"), body = (Assign {var = "kek", expr = (Num 0)}) })
  runParser2 parseWhile "While > - x 5 0 Write x"
   (While {cond = (BinOp Gt (BinOp Minus (Ident "x") (Num 5)) (Num 0)), body = (Write {expr = (Ident "x")}) })

  assertBool "" $ isFailure $ runParser parseWhile "If x Write 1 Write 0"
  assertBool "" $ isFailure $ runParser parseWhile "while kek mem"
  assertBool "" $ isFailure $ runParser parseWhile "Privet mir!"



unit_parseSeq :: Assertion
unit_parseSeq = do
  runParser2 parseSeq "Seq Read x Write x"
   (Seq [(Read {var = "x"}), (Write {expr = (Ident "x")})])

  assertBool "" $ isFailure $ runParser parseSeq "Seq Write 0 Write"
  assertBool "" $ isFailure $ runParser parseSeq "Seq"
  assertBool "" $ isFailure $ runParser parseSeq "Seq vsem_privet"


unit_parseSeqNop :: Assertion
unit_parseSeqNop = do
  runParser2 parseSeqNop "Nop   "
      (Seq [])
  runParser2 parseSeqNop "Nop"
      (Seq [])

  assertBool "" $ isFailure $ runParser parseSeqNop "Nop Write 0 Write 1"
  assertBool "" $ isFailure $ runParser parseSeqNop "Nop Read x Write x"
  assertBool "" $ isFailure $ runParser parseSeqNop "Nop vsem_privet"


unit_parseL :: Assertion
unit_parseL = do
  runParser2 parseL "If xx Write 1 Write 0 "
   (If {cond = (Ident "xx"), thn = (Write {expr = (Num 1)}), els = (Write {expr = (Num 0)}) })
  runParser2 parseL "Seq Read X If > X 13 Write X While < X 42 Seq Assign X * X 7 Write X"
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
  runParser2 parseL "Seq Seq Seq Read n Assign cur 1 While > n 0 Seq Assign cur * cur n Assign n - n 1 Write n"
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

  runParser2 parseL "Seq Nop Nop" (Seq [Seq[], Seq[]])
  runParser2 parseL "Seq Nop Seq Nop Seq Nop Nop"
   (Seq [Seq [], Seq [Seq[], Seq[Seq[], Seq[]]]])

  runParser2 parseL "Seq Seq Seq Read n If > n 0 Write n Nop Write * n -- 1 Write ! n"
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

  runParser2 parseL "Write ! 1" (Write (UnaryOp Not (Num 1)))
  runParser2 parseL "Write 1" (Write (Num 1))
  runParser2 parseL "Read read" (Read "read")
  runParser2 parseL "If ! x Nop Nop" (If (UnaryOp Not (Ident "x")) (Seq []) (Seq []))
  runParser2 parseL "If -- x Nop Nop" (If (UnaryOp Minus (Ident "x")) (Seq []) (Seq []))

  assertBool "" $ isFailure $ runParser parseL "Write Read"
  assertBool "" $ isFailure $ runParser parseL "If - x Nop Nop"
  assertBool "" $ isFailure $ runParser parseL "If  Nop Nop"
  assertBool "" $ isFailure $ runParser parseL "If  "
  assertBool "" $ isFailure $ runParser parseL "Seq Read a While x y"
  assertBool "" $ isFailure $ runParser parseL "shto proishodit?"
  assertBool "" $ isFailure $ runParser parseL "If a > 0 then a := -1 else a := 1"
  assertBool "" $ isFailure $ runParser parseL "Seq Nop"



unit_parseDef :: Assertion
unit_parseDef = do
  runParser2 parseDef "Fun Privet With a With b ThatsAll Nop Return + a b"
    (Function "Privet" ["a", "b"] (Seq[]) ((BinOp Plus (Ident "a") (Ident "b"))))
  -- runParser2 parseDef "Fun Proc ThatsAll Seq Read x Write x"
  --   (Function "Proc" [] (Seq [(Read "x"), (Write (Ident "x"))]))
  -- runParser2 parseDef "Fun _ ThatsAll Return 239"
  --   (Function "_" [] (Return (Num 239)))

  assertBool "" $ isFailure $ runParser parseDef "Return "
  assertBool "" $ isFailure $ runParser parseDef "Fun x With a With b Return + a b"
  assertBool "" $ isFailure $ runParser parseDef "Fun privet 2"
  assertBool "" $ isFailure $ runParser parseDef "Fun fail ThatsAll Return 0"






program0 = unlines
    [
      "        WithFun",
      "        Fun One ThatsAll",
      "            Write 1",
      "            Return 0",
      "        ThatsAllFun",
      "        Write Call One ThatsAll"
    ]

program1 = unlines
    [
        "        WithFun ",
        "        Fun Sum With a With b ThatsAll",
        "            Nop",
        "            Return + a b",
        "        WithFun",
        "        Fun UnarMinus With a ThatsAll",
        "            Nop",
        "            Return -- a ",
        "        WithFun",
        "        Fun SayTwentyAndReturnTen ThatsAll",
        "            Write 20",
        "            Return 10",
        "        ThatsAllFun",
        "        Seq Seq Seq Seq",
        "            Read a",
        "            Read b",
        "            Write Call Sum With a With b ThatsAll",
        "            Assign kek Call UnarMinus With 5 ThatsAll",
        "            Assign mem Call SayTwentyAndReturnTen ThatsAll"
    ]


unit_parseProg :: Assertion
unit_parseProg = do
  runParser2 parseProg program0
    (Program {
      functions = [
          (Function "One" [] (Write (Num 1)) (Num 0))
      ],
      main = (Write (FunctionCall "One" []))
    })


  runParser2 parseProg program1
      (Program {
        functions = [
            (Function "Sum" ["a", "b"] (Seq []) ((BinOp Plus (Ident "a") (Ident "b")))),
            (Function "UnarMinus" ["a"] (Seq []) ((UnaryOp Minus (Ident "a")))),
            (Function "SayTwentyAndReturnTen" [] (Write (Num 20)) (Num 10))
        ],
        main = (Seq [Seq [Seq [Seq [
            (Read "a"),
            (Read "b")
            ],
            (Write (FunctionCall "Sum" [Ident "a", Ident "b"]))
            ],
            (Assign "kek" (FunctionCall "UnarMinus" [(Num 5)]))
            ],
            (Assign "mem" (FunctionCall "SayTwentyAndReturnTen" []))
            ])
      })

  runParser2 parseProg "ThatsAllFun Write 5" (Program [] (Write (Num 5)))

  assertBool "" $ isFailure $ runParser parseProg "WithFun Fun Sum With a With b ThatsAll Return + a b ThatsAll "
  assertBool "" $ isFailure $ runParser parseProg "mem"
  assertBool "" $ isFailure $ runParser parseProg "  "

  --
