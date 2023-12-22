module Main (main) where

import qualified Parse.ForSpec (allTests)
import qualified Parse.PrintSpec (allTests)
import qualified Parse.IfSpec (allTests)
import qualified SimpleProgramSpec
import qualified ExprSpec (allTests)
import Test.Hspec


main :: IO ()
main = hspec $ do
--  ParseDefsSpec.allTests
  ExprSpec.allTests
  Parse.PrintSpec.allTests
  Parse.ForSpec.allTests
  Parse.IfSpec.allTests

  SimpleProgramSpec.allTests

