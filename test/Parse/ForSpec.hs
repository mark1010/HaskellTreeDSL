{-# LANGUAGE OverloadedStrings #-}
module Parse.ForSpec (main,allTests) where
import Parser.Parser( program, Expr(..), Stmt(..) )
import Parser.Stmt ( forStmt, LineNumber(LineNumber) )
import Test.Hspec ( hspec, describe, it, Spec )
import Test.Hspec.Megaparsec ( shouldParse )
import Text.Megaparsec ( parse )



forStmtTests :: Spec
forStmtTests = describe "FOR statement parser" $ do

    -- NOTE WE CAN'T HAVE A FOR STATEMENT WITHOUT A NEXT STATEMENT
    it "parses valid FOR statement" $ do
      let program0 = "10 FOR i = 10 TO 20\n20 NEXT i\n"
      -- testPrint program0
      let expected0 = For (LineNumber 10) "i" (ConstInt 10) (ConstInt 20) []
      parse forStmt "" program0 `shouldParse` expected0


programTests :: Spec
programTests = describe "program Parser" $ do

    it "parses valid FOR statement in program" $ do
      let program1 =
            unlines
              [ "10 FOR i = 1 TO 5",
                "20 FOR j = 6 TO 10",
                "30 REM test",
                "60 NEXT j",
                "70 NEXT i"
              ]
      -- testPrint program1
      let expected0 =
            [ For
                (LineNumber 10)
                "i"
                (ConstInt 1)
                (ConstInt 5)
                [ For
                    (LineNumber 20)
                    "j"
                    (ConstInt 6)
                    (ConstInt 10)
                    [ Rem (LineNumber 30) "test"
                    ]
                ]
            ]
      parse program "" program1 `shouldParse` expected0


forProgramTests :: Spec
forProgramTests = describe "FOR NEXT program tests" $ do
  it "parses valid FOR statement in program, with PRINT" $ do
    let program1 =
          unlines
            [ "10 FOR i = 1 TO 5",
              "20 FOR j = 6 TO 10",
              "30 PRINT i",
              "60 NEXT j",
              "70 NEXT i"
            ]
    -- testPrint program1
    let expected0 =
          [ For
              (LineNumber 10)
              "i"
              (ConstInt 1)
              (ConstInt 5)
              [ For
                  (LineNumber 20)
                  "j"
                  (ConstInt 6)
                  (ConstInt 10)
                  [ Print (LineNumber 30) (Var "i") True
                  ]
              ]
          ]
    parse program "" program1 `shouldParse` expected0

printTests :: Spec
printTests = describe "PRINT tests" $ do
  it "parses valid PRINT statements in program" $ do
    let program1 =
          unlines
            [ "10 PRINT \"test\"",
              "20 PRINT 100",
              "30 PRINT i"
            ]
    let expected0 =
          [ Print (LineNumber 10) (ConstStr "test") True,
            Print (LineNumber 20) (ConstInt 100) True,
            Print (LineNumber 30) (Var "i") True
          ]
    parse program "" program1 `shouldParse` expected0

  it "parses valid PRINT statements in program with semicolon" $ do
    let program1 =
          unlines
            [ "10 PRINT \"test\";",
              "20 PRINT 100;",
              "30 PRINT i;"
            ]
    -- testPrint program1
    let expected0 =
          [ Print (LineNumber 10) (ConstStr "test") False,
            Print (LineNumber 20) (ConstInt 100) False,
            Print (LineNumber 30) (Var "i") False
          ]
    parse program "" program1 `shouldParse` expected0


allTests :: Spec
allTests = do
  forStmtTests
  forProgramTests


main :: IO ()
main = hspec $ do
  forStmtTests
  forProgramTests
