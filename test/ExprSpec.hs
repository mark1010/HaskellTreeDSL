module ExprSpec (main, allTests) where

import Test.Hspec
import Text.Megaparsec
import Test.Hspec.Megaparsec
-- import ParseDefs -- Your module with the parsers
import Parser.Expr
import Parser.Parser

termParserTests :: Spec
termParserTests =
  describe "term parser" $ do

    describe "multiplication" $ do

      it "parses simple multiplication, no spaces" $ do
        let exptected = Mult (ConstInt 3) (ConstInt 4)
        parse term "" "3*4" `shouldParse` exptected

      it "parses simple multiplication" $ do
        let exptected = Mult (ConstInt 3) (ConstInt 4)
        parse term "" "3 * 4" `shouldParse` exptected

      it "parses simple multiplication, no spaces" $ do
        let exptected = Mult (ConstInt 3) (ConstInt 4)
        parse term "" "3* 4" `shouldParse` exptected

      it "parses simple multiplication, no spaces" $ do
        let exptected = Mult (ConstInt 3) (ConstInt 4)
        parse term "" "3 *4" `shouldParse` exptected

      it "parses simple multiplication, no spaces, three parameters" $ do
        let exptected = Mult (Mult (ConstInt 3) (ConstInt 4)) (ConstInt 5)
        parse term "" "3*4*5" `shouldParse` exptected

    describe "divsion" $ do

      it "12 divided by 4, no spaces" $ do
        let exptected = Div (ConstInt 12) (ConstInt 4)
        parse term "" "12/4" `shouldParse` exptected


      it "12 divided by i, no spaces" $ do
        let exptected = Div (ConstInt 12) (Var "i")
        parse term "" "12/i" `shouldParse` exptected



arithParserTests :: Spec
arithParserTests =
  describe "term parser" $ do

    describe "addition" $ do
      it "12 plus 4" $ do
        let exptected = Add (ConstInt 12) (ConstInt 4)
        parse arithExpr "" "12 + 4" `shouldParse` exptected

      it "12 plus 4, no spaces" $ do
        let exptected = Add (ConstInt 12) (ConstInt 4)
        parse arithExpr "" "12+4" `shouldParse` exptected

      it "12 plus 4 plus 3" $ do
        let exptected = Add (Add (ConstInt 12) (ConstInt 4)) (ConstInt 3)
        parse arithExpr "" "12 + 4 + 3" `shouldParse` exptected


    describe "subtraction" $ do
      it "12 minus 4" $ do
        let exptected = Sub (ConstInt 12) (ConstInt 4)
        parse arithExpr "" "12 - 4" `shouldParse` exptected

      it "12 minus 4, no spaces" $ do
        let exptected = Sub (ConstInt 12) (ConstInt 4)
        parse arithExpr "" "12-4" `shouldParse` exptected

      it "12 minus 4 minus 3" $ do
        let exptected = Sub (Sub (ConstInt 12) (ConstInt 4)) (ConstInt 3)
        parse arithExpr "" "12 - 4 - 3" `shouldParse` exptected


opPrecParserTests :: Spec
opPrecParserTests = do
  describe "op prec parser" $ do

    describe "mixed" $ do
      it "12 + 4 * 3" $ do
        let exptected = Add (ConstInt 12) (Mult (ConstInt 4) (ConstInt 3))
        parse arithExpr "" "12 + 4 * 3" `shouldParse` exptected




varExprTests :: Spec
varExprTests = 
  describe "varExpr Parser" $ do
    it "parses valid variable expressions" $ do
      parse varExpr "" "variable " `shouldParse` Var "variable"
    
    it "fails to parse invalid variable expressions" $ do
      parse varExpr "" `shouldFailOn` "123invalid "

    it "fails to parse invalid variable expressions" $ do
      parse varExpr "" `shouldFailOn` "123invalid%"

    it "parses variable expressions without trailing space" $ do
      parse varExpr "" "variable123" `shouldParse` Var "variable123"


stringExprTests :: Spec
stringExprTests = describe "stringExpr and stringLiteral Parser" $ do

  describe "stringExpr and stringLiteral Parser" $ do
    it "parses valid string expressions" $ do
      parse stringExpr "" "\"Hello, World!\"" `shouldParse` ConstStr "Hello, World!"

    it "parses empty string expressions" $ do
      parse stringExpr "" "\"\"" `shouldParse` ConstStr ""

    it "fails to parse invalid string expressions" $ do
      parse stringExpr "" `shouldFailOn` "Hello, World!"

    it "fails to parse incomplete string expressions" $ do
      parse stringExpr "" `shouldFailOn` "\"Hello, World"



numberExprTests :: Spec
numberExprTests = describe "numberExprTest Parser" $ do

  describe "number Parser" $ do
    it "parses positive integers followed by space" $ do
      parse number "" "123 " `shouldParse` 123

    it "parses positive integers without trailing space" $ do
      parse number "" "123" `shouldParse` 123

    it "fails to parse non-integer input" $ do
      parse number "" `shouldFailOn` "abc"

  describe "unaryExpr Parser" $ do
    it "parses negative integers" $ do
      parse unaryExpr "" "-123 " `shouldParse` Negate (ConstInt 123)


    it "fails to parse negative integers with space" $ do
      parse unaryExpr "" `shouldFailOn` "- 123"


    it "parses negative variables" $ do
      parse unaryExpr "" "-variable " `shouldParse` Negate (Var "variable")


    it "fails to parse non-unary expressions" $ do
      parse unaryExpr "" `shouldFailOn` "123"


constExprTests :: Spec
constExprTests = describe "constExprTests parser" $ do

  describe "ConstInt Parser" $ do
    it "parses positive integer, space after" $ do
      parse constExpr "" "123 " `shouldParse` ConstInt 123

  describe "ConstInt Parser" $ do
    it "parses positive integer, no spaces" $ do
      parse constExpr "" "890" `shouldParse` ConstInt 890

  describe "ConstInt Parser" $ do
    it "parses positive integer, space before" $ do
      parse constExpr "" `shouldFailOn` " 890"

  describe "ConstInt Parser" $ do
    it "parses positive integer, space before and after" $ do
      parse constExpr "" `shouldFailOn` " 899 " 



conditionTests :: Spec
conditionTests = describe "expr parser - conditions" $ do

    it "parses a = 1" $ do
      parse expr "" "a = 1" `shouldParse` Eq (Var "a") (ConstInt 1)

    it "parses a=1" $ do
      parse expr "" "a=1" `shouldParse` Eq (Var "a") (ConstInt 1)


exprTests :: Spec
exprTests = describe "expr parser" $ do

    it "parses positive integers followed by space" $ do
      parse expr "" "123 " `shouldParse` ConstInt 123

    it "parses negative variables" $ do
      parse expr "" "-variable " `shouldParse` Negate (Var "variable")

    it "fails to parse negative integers with space" $ do
      parse expr "" `shouldFailOn` "- 123"

    it "parses negative variables" $ do
      parse expr "" "-variable " `shouldParse` Negate (Var "variable")

    it "parses valid string expressions" $ do
      parse expr "" "\"Hello, World!\"" `shouldParse` ConstStr "Hello, World!"


andTests :: Spec
andTests = describe "expr AND parser" $ do

    it "AND const const" $ do
      parse expr "" "123 AND 456" `shouldParse` And (ConstInt 123) (ConstInt 456)

    it "AND var=0 const" $ do
      let expected = And (Eq (Var "i") (ConstInt 0)) (ConstInt 456)
      parse expr "" "i = 0 AND 456" `shouldParse` expected

    it "AND var=0 var=1" $ do
      let expected = And (Eq (Var "i") (ConstInt 0)) (Eq (Var "o") (ConstInt 10)) 
      parse expr "" "i = 0 AND o = 10" `shouldParse` expected



allTests :: Spec
allTests = do
  numberExprTests
  constExprTests
  termParserTests
  arithParserTests
  opPrecParserTests
  varExprTests
  stringExprTests  
  exprTests
  conditionTests
  andTests


main :: IO ()
main = hspec $ do 
  allTests
  --andTests
