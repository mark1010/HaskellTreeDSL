{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module SimpleProgramSpec (allTests, main) where

import Parser.Parser
import Parser.Stmt
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

testPrint :: String -> IO ()
testPrint prg = do
  putStrLn prg
  case parseBasicProgram prg of
    Left bundle -> putStr (errorBundlePretty bundle) -- Print parsing errors
    Right ast -> print ast -- Print the abstract syntax tree (AST)

letTests :: Spec
letTests = describe "LET statement parser" $ do

    it "parses valid LET statement" $ do
      let program0 = "10 LET i = 1\n"
      -- testPrint program0
      let expected0 = Let (LineNumber 10) "i" (ConstInt 1)
      parse letStmt "" program0 `shouldParse` expected0

    it "parses valid LET statement with arithmetic" $ do
      let program0 = "10 LET i = 10 - 5\n"
      -- testPrint program0
      let expected0 = [Let (LineNumber 10) "i" (Sub (ConstInt 10) (ConstInt 5))]
      parse program "" program0 `shouldParse` expected0

    it "parses valid LET statement, followed by LET" $ do
      let program0 = "10 LET i = 1\n20 LET j = 10\n"
      -- testPrint program0
      let expected0 = [Let (LineNumber 10) "i" (ConstInt 1), Let (LineNumber 20) "j" (ConstInt 10)]
      parse program "" program0 `shouldParse` expected0


programTests :: Spec
programTests = describe "program Parser" $ do

    it "parses valid LET statement" $ do
      let program0 = "10 LET i = 1\n"
      -- testPrint program0
      let expected0 = [Let (LineNumber 10) "i" (ConstInt 1)]
      parse program "" program0 `shouldParse` expected0

    it "parses valid LET statement, followed by LET" $ do
      let program0 = "10 LET i = 1\n20 LET j = 10\n"
      -- testPrint program0
      let expected0 = [Let (LineNumber 10) "i" (ConstInt 1), Let (LineNumber 20) "j" (ConstInt 10)]
      parse program "" program0 `shouldParse` expected0

    it "parses valid LET statement" $ do
      let program0 = "10 LET i = 100\n"
      -- testPrint program0
      let expected0 = [Let (LineNumber 10) "i" (ConstInt 100)]
      parse program "" program0 `shouldParse` expected0


    it "parses valid FOR statement in program" $ do
      let program0 = "10 FOR i = 10 TO 20\n20 NEXT i\n"
      testPrint program0
      let expected0 = [For (LineNumber 10) "i" (ConstInt 10) (ConstInt 20) []]
      parse program "" program0 `shouldParse` expected0

    it "parses valid REM statement" $ do
      let program0 = "10 REM test\n"
      testPrint program0
      let expected0 = Rem (LineNumber 10) "test"
      parse remStmt "" program0 `shouldParse` expected0

    it "parses valid REM statement with following text" $ do
      let program0 = "10 REM test\n20 LET i = 1\n"
      testPrint program0
      let expected0 = Rem (LineNumber 10) "test"
      parse remStmt "" program0 `shouldParse` expected0

    it "parses valid REM statement with following text (program)" $ do
      let program0 = "10 REM test\n20 LET i = 1\n"
      testPrint program0
      let expected0 = [Rem (LineNumber 10) "test", Let (LineNumber 20) "i" (ConstInt 1)]
      parse program "" program0 `shouldParse` expected0

    it "parses valid FOR statement in program, with REM" $ do
      let program0 = "10 FOR i = 10 TO 20\n20 REM test\n30 NEXT i\n"
      testPrint program0
      let expected0 = [For (LineNumber 10) "i" (ConstInt 10) (ConstInt 20) [Rem (LineNumber 20) "test"]]
      parse program "" program0 `shouldParse` expected0

    it "parses valid FOR statement in program" $ do
      let program1 =
            unlines
              [ "10 FOR i = 1 TO 5",
                "20 FOR j = 6 TO 10",
                "30 REM test",
                "60 NEXT j",
                "70 NEXT i"
              ]
      testPrint program1
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



progamLetTests :: Spec
progamLetTests =
  describe "program tests basic FOR LET and PRINT" $ do
    it "parses a basic PRINT statement followed by LET" $ do
      let input =
            unlines
              [ "10 PRINT \"word\"",
                "20 LET x = 1"
              ]
      let expected =
            [ Print (LineNumber 10) (ConstStr "word") True,
              Let (LineNumber 20) "x" (ConstInt 1)
            ]
      putStrLn input
      case parseBasicProgram input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right result -> result `shouldBe` expected

    it "parses a basic PRINT statement, with trailing spaces, followed by LET" $ do
      let input =
            unlines
              [ "10 PRINT \"word\"       ",
                "20 LET x = 1"
              ]
      let expected =
            [ Print (LineNumber 10) (ConstStr "word") True,
              Let (LineNumber 20) "x" (ConstInt 1)
            ]
      putStrLn input
      case parseBasicProgram input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right result -> result `shouldBe` expected


progamForPrintTests :: Spec
progamForPrintTests =

    describe "program tests FOR and PRINT" $ do

      -- loop tests
      it "parses FOR statement with PRINT" $ do
        let input =
              unlines
                [ "10 FOR i = 1 TO 10",
                  "20 PRINT \"word\"",
                  "30 NEXT i"
                ]
        let expected = [For (LineNumber 10) "i" (ConstInt 1) (ConstInt 10) [Print (LineNumber 20) (ConstStr "word") True]]
        case parseBasicProgram input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right result -> result `shouldBe` expected

      it "parses FOR statement with PRINT i" $ do
        let input =
              unlines
                [ "10 FOR i = 1 TO 10",
                  "20 PRINT i",
                  "30 NEXT i"
                ]
        let expected = [For (LineNumber 10) "i" (ConstInt 1) (ConstInt 10) [Print (LineNumber 20) (Var "i") True]]
        case parseBasicProgram input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right result -> result `shouldBe` expected

      it "parses FOR statement with PRINT i;" $ do
        let input =
              unlines
                [ "10 FOR i = 1 TO 10",
                  "20 PRINT i;",
                  "30 NEXT i"
                ]
        let expected = [For (LineNumber 10) "i" (ConstInt 1) (ConstInt 10) [Print (LineNumber 20) (Var "i") False]]
        case parseBasicProgram input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right result -> result `shouldBe` expected

      it "parses FOR statement with empty PRINT" $ do
        let input =
              unlines
                [ "10 FOR i = 1 TO 10",
                  "20 PRINT",
                  "30 NEXT i"
                ]
        let expected = [For (LineNumber 10) "i" (ConstInt 1) (ConstInt 10) [Print (LineNumber 20) (ConstStr "") True]]
        case parseBasicProgram input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right result -> result `shouldBe` expected

      it "parses FOR statement with empty string in PRINT ;" $ do
        let input =
              unlines
                [ "10 FOR i = 1 TO 10",
                  "20 PRINT ;",
                  "30 NEXT i"
                ]
        let expected = [For (LineNumber 10) "i" (ConstInt 1) (ConstInt 10) [Print (LineNumber 20) (ConstStr "") False]]
        case parseBasicProgram input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right result -> result `shouldBe` expected



colorTests :: Spec
colorTests = describe "color Parser" $ do
    it "parses valid COLOR statement" $ do
      let program0 = "10 COLOR \"Yellow\"\n"
      -- testPrint program0
      let expected = [Color (LineNumber 10) "Yellow"]
      parse program "" program0 `shouldParse` expected


allTests :: Spec
allTests = do
  letTests
  programTests
  progamLetTests
  progamForPrintTests
  colorTests


main :: IO ()
main = hspec $ do
  allTests
