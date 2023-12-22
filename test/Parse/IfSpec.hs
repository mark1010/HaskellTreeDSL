module Parse.IfSpec (main,allTests) where

import Test.Hspec ( hspec, describe, it, Spec )
import Text.Megaparsec
import Test.Hspec.Megaparsec ( shouldParse )
import Parser.Expr
import Parser.Stmt
import Parser.Parser

ifThenTests :: Spec
ifThenTests =
  describe "IF THEN END" $ do
    it "parses empty IF ELSE END" $ do
      let input = unlines 
            [ 
              "30 IF a = 0 THEN",
              "50 ELSE",
              "60 END"
            ]

      let expected = If (LineNumber 30) (Eq (Var "a") (ConstInt 0)) [] (Just [])
      parse ifStmt "" input `shouldParse` expected

    it "parses IF END no ELSE" $ do
      let input = unlines 
            [ 
              "30 IF a = 0 THEN",
              "60 END"
            ]

      let expected = If (LineNumber 30) (Eq (Var "a") (ConstInt 0)) [] Nothing
      parse ifStmt "" input `shouldParse` expected


ifThenProgramTests :: Spec
ifThenProgramTests = describe "IF THEN tests" $ do
  it "parses valid IF THEN END statements in program" $ do
    let input = unlines 
            [ 
              "20 LET a = 0",
              "30 IF a = 0 THEN",
              "50 REM PRINT \"x\";",
              "60 END"
            ]
    let expected =
          [Let (LineNumber 20) "a" (ConstInt 0), 
          If (LineNumber 30) (Eq (Var "a") (ConstInt 0)) [Rem (LineNumber 50) "PRINT \"x\";"] Nothing
          ] 
    parse program "" input `shouldParse` expected





  it "parses valid PRINT statements in program with semicolon" $ do
    let input = unlines 
            [ 
              "20 LET a = 0",
              "30 IF a = 0 THEN",
              "50 PRINT \"x\";",
              "60 END"
            ]
    let expected0 =
           [Let (LineNumber 20) "a" (ConstInt 0), 
           If (LineNumber 30) (Eq (Var "a") (ConstInt 0)) [Print (LineNumber 50) (ConstStr  "x") False] Nothing]
    parse program "" input `shouldParse` expected0


ifThenProgramElseTests :: Spec
ifThenProgramElseTests = describe "IF THEN ELSE tests" $ do


  it "parses valid IF THEN ELSE END statements in program" $ do
    let input = unlines 
            [ 
              "20 LET a = 0",
              "30 IF a = 0 THEN",
              "50 REM In body",
              "60 ELSE",
--              "70 REM In else",
              "70 PRINT",
              "80 END"
            ]
    let expected =
          [ Let (LineNumber 20) "a" (ConstInt 0), 
            If (LineNumber 30) (Eq (Var "a") (ConstInt 0)) [Rem (LineNumber 50) "In body"] (Just [Print (LineNumber 70) (ConstStr "") True])
          ] 
    putStrLn input
    parse program "" input `shouldParse` expected


  it "parses valid IF THEN ELSE END statements in program, with code following" $ do
    let input = unlines 
            [ 
              "20 LET a = 0",
              "30 IF a = 0 THEN",
              "50 REM In body",
              "60 ELSE",
              "70 PRINT",
              "80 END",
              "90 PRINT \"After\""
            ]
    let expected =
          [ Let (LineNumber 20) "a" (ConstInt 0), 
            If (LineNumber 30) (Eq (Var "a") (ConstInt 0)) [Rem (LineNumber 50) "In body"] (Just [Print (LineNumber 70) (ConstStr "") True]),
            Print (LineNumber 90) (ConstStr "After") True
          ] 
    putStrLn input
    parse program "" input `shouldParse` expected

  it "parses valid IF THEN ELSE END, multiple statements in ELSE" $ do
    let input = unlines 
            [ 
              "20 LET a = 0",
              "30 IF a = 0 THEN",
              "50 REM In body",
              "60 ELSE",
              "70 PRINT",
              "75 PRINT a",
              "80 END",
              "90 PRINT \"After\""
            ]
    let expected =
          [ Let (LineNumber 20) "a" (ConstInt 0), 
            If (LineNumber 30) (Eq (Var "a") (ConstInt 0)) 
              [Rem (LineNumber 50) "In body"] 
              (Just [Print (LineNumber 70) (ConstStr "") True,
                Print (LineNumber 75) (Var "a") True
              ]),
            Print (LineNumber 90) (ConstStr "After") True
          ] 
    putStrLn input
    parse program "" input `shouldParse` expected



  it "parses valid IF THEN ELSE END, nested IF THEN END in ELSE" $ do
    let input = unlines 
            [ 
              "20 LET a = 0",
              "30 IF a = 0 THEN",
              "50 REM In body",
              "60 ELSE",
              "65 IF a = 1 THEN",
              "70 PRINT \"a is 1\"",
              "75 END",
              "80 END",
              "90 PRINT \"After\""
            ]
    let expected =
          [ Let (LineNumber 20) "a" (ConstInt 0), 
            If (LineNumber 30) (Eq (Var "a") (ConstInt 0)) 
              [Rem (LineNumber 50) "In body"] 
              (Just [If (LineNumber 65) (Eq (Var "a") (ConstInt 1)) 
                [ Print (LineNumber 70) (ConstStr "a is 1") True] Nothing]), 
            Print (LineNumber 90) (ConstStr "After") True]

    putStrLn input
    parse program "" input `shouldParse` expected



allTests :: Spec
allTests = do
  ifThenTests
  ifThenProgramTests
  ifThenProgramElseTests


main :: IO ()
main = hspec $ do
  allTests  
  -- ifThenTests
  -- ifThenProgramTests
  -- ifThenProgramElseTests
