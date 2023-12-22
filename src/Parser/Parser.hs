module Parser.Parser (program, parseBasicProgram, parseBasicProgram', Stmt(..), Expr(..)) where

-- import Debug.Trace (traceShow, traceShowId)

import Control.Monad.Combinators
import Data.Void
import Parser.Common
import Parser.Stmt

import Text.Megaparsec
import Text.Megaparsec.Char



program :: Parser [Stmt]
program = space >> many (stmt <* space)

parseBasicProgram :: String -> Either (ParseErrorBundle String Void) [Stmt]
parseBasicProgram = parse program ""


parseBasicProgram' :: String -> Either (ParseErrorBundle String Void) [Stmt]
parseBasicProgram' prog  = parse program "" ("0 PRINT\n" ++ prog)
