module Parse.PrintSpec (main,allTests) where

import Test.Hspec
import Text.Megaparsec
import Test.Hspec.Megaparsec

import Parser.Parser
import Parser.Expr
import Parser.Stmt
import Parser.Common
import Control.Monad.RWS (MonadState(put))




-- line function to add \n to end of string
line :: String -> String
line s = s ++ "\n" -- ++ "20 REM this is a comment\n"

printTests :: Spec
printTests =
  describe "PRINT printStmt parser" $ do
    it "parses a basic print statement correctly" $ do
      let input = line "10 PRINT \"Hello, World!\""
      let expected = Print (LineNumber 10) (ConstStr "Hello, World!") True
      parse printStmt "" input `shouldParse` expected

    it "parses a basic print statement with semi-colon" $ do
      let input = line "10 PRINT \"Hello, World!\";"
      let expected = Print (LineNumber 10) (ConstStr "Hello, World!") False
      parse printStmt "" input `shouldParse` expected

    it "parses a basic print statement with trailing spaces" $ do
      let input = line "10 PRINT \"Hello, World!\"           "
      let expected = Print (LineNumber 10) (ConstStr "Hello, World!") True
      parse printStmt "" input `shouldParse` expected

    it "parses a basic print statement with empty quotes" $ do
      let input = line "10 PRINT \"\""
      let expected = Print (LineNumber 10) (ConstStr "") True
      parse printStmt "" input `shouldParse` expected

    it "parses a basic print statement with empty quotes and semi-colon" $ do
      let input = line "10 PRINT \"\";"
      let expected = Print (LineNumber 10) (ConstStr "") False
      parse printStmt "" input `shouldParse` expected

    it "parses a basic PRINT statement with empty line" $ do
      let input = line "10 PRINT"
      let expected = Print (LineNumber 10) (ConstStr "") True
      parse printStmt "" input `shouldParse` expected

    it "parses a basic PRINT statement with empty line and semi-colon" $ do
      let input = line "10 PRINT ;"
      let expected = Print (LineNumber 10) (ConstStr "") False
      parse printStmt "" input `shouldParse` expected

    it "parses a basic PRINT variable" $ do
      let input = line "10 PRINT i"
      let expected = Print (LineNumber 10) (Var "i") True
      parse printStmt "" input `shouldParse` expected

    it "parses a basic PRINT j variable with new line supressed" $ do
      let input = line "10 PRINT j;"
      let expected = Print (LineNumber 10) (Var "j") False
      parse printStmt "" input `shouldParse` expected

    it "parses a basic PRINT j variable with new line supressed, spaced" $ do
      let input = line "10 PRINT j ;"
      let expected = Print (LineNumber 10) (Var "j") False
      parse printStmt "" input `shouldParse` expected

    -- it "parses a basic PRINT j variable with new line supressed, followed by other content" $ do
    --   let input = "10 PRINT j ; \n20 REM this is a comment\n"
    --   let expected = Print (LineNumber 10) (Var "j") False
    --   parse printStmt "" input `shouldParse` expected


printProgramTests :: Spec
printProgramTests = describe "PRINT tests" $ do
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
            [ "10 PRINT \"test\";"
            , "20 PRINT 100;"
            , "30 PRINT i;"
            ]
    --testPrint program1
    -- putStrLn program1
    let expected0 =
          [ Print (LineNumber 10) (ConstStr "test") False,
            Print (LineNumber 20) (ConstInt 100) False,
            Print (LineNumber 30) (Var "i") False
          ]
    parse program "" program1 `shouldParse` expected0




allTests :: Spec
allTests = do
  printTests
  printProgramTests


main :: IO ()
main = hspec allTests