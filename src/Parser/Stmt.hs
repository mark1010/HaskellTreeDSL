{-# LANGUAGE OverloadedStrings #-}

module Parser.Stmt
  ( 
    Stmt (..),
    Expr (..),
    LineNumber (..),
    stmt,
    -- internal:
    printStmt,
    forStmt,
    remStmt,
    letStmt,
    ifStmt
  )
where

import Control.Monad (when)
import Control.Monad.Combinators
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, isNothing)
import Parser.Common
import Parser.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-- import Text.Megaparsec.Debug (dbg)
-- import Debug.Trace (traceShow, traceShowId)

-- Statements

newtype LineNumber = LineNumber Int
  deriving (Eq, Show)

lineNumber :: Parser LineNumber
lineNumber = LineNumber <$> L.decimal <* space

data Stmt
  = Let LineNumber String Expr
  | For LineNumber String Expr Expr [Stmt]
  | Print LineNumber Expr Bool -- Bool indicates whether to add a newline
  -- | If LineNumber Expr [Stmt]
  | If LineNumber Expr [Stmt] (Maybe [Stmt]) -- Added Maybe [Stmt] for ELSE branch
  | Rem LineNumber String
  | Color LineNumber String -- foreground / text color
  deriving (Show, Eq)


stmt :: Parser Stmt
stmt =
    try printStmt
    <|> try letStmt
    <|> try forStmt
    <|> try ifStmt
    <|> try remStmt
    <|> try colorStmt
    <|> unknownStmt


keywordRem :: Parser String
keywordRem = string "REM" <* space

remStmt :: Parser Stmt
remStmt = do
    ln <- lineNumber
    _ <- keywordRem
    comment <- takeWhileP (Just "Comment") (/= '\n') -- Consumes the rest of the line
    _ <- eol -- Explicitly consume the newline
    return $ Rem ln comment



colorStmt :: Parser Stmt
colorStmt = do
    ln <- lineNumber
    _ <- string "COLOR" <* space
    -- color <- takeWhileP (Just "Color") (/= '\n') 
    expr' <- stringLiteral
    _ <- eol -- Explicitly consume the newline
    return $ Color ln expr'



keywordLet :: Parser String
keywordLet = string "LET" <* space

letStmt :: Parser Stmt
letStmt = do
    ln <- lineNumber
    _ <- keywordLet
    varName <- identifier
    _ <- space
    _ <- string "=" <* space
    exprResult <- expr --constExpr <|> varExpr 
    _ <- eol -- Explicitly consume the newline
    return $ Let ln varName exprResult

keywordFor :: Parser String
keywordFor = string "FOR" <* space

keywordTo :: Parser String
keywordTo = string "TO" <* space

keywordNext :: Parser String
keywordNext = string "NEXT" <* space1

keywordNextWithLine :: Parser ()
keywordNextWithLine =  do
  _ <- lineNumber -- This line number is for the 'END' keyword
  _ <- keywordNext
  return ()


forStmt :: Parser Stmt
forStmt = do

  ln <- lineNumber
  _ <- keywordFor
  varName <- identifier

  _ <- space
  _ <- string "=" <* space
  startExpr <- constExpr <|> varExpr 
  _ <- space
  _ <- keywordTo
  endExpr <- constExpr <|> varExpr 

  _ <- eol -- Explicitly consume the newline

  -- Parse the loop body: a list of statements until NEXT is encountered
  loopBody <- manyTill stmt (try keywordNextWithLine)

  -- Parse the variable name after NEXT, and ensure it matches the loop variable
  nextVarName <- identifier
  when (nextVarName /= varName) $
    fail $
      "Loop variable mismatch: expected " ++ varName ++ ", got " ++ nextVarName

  _ <- eol -- Explicitly consume the newline after NEXT
  return $ For ln varName startExpr endExpr loopBody




printStmt :: Parser Stmt
printStmt = do
  ln <- lineNumber
  _ <- string "PRINT" 
  _ <- optional (char ' ')
  expr' <- optional (expr <* notFollowedBy (oneOf ("+-" :: String)))
  _ <- many (char ' ')
  semicolon <- optional (char ';')
  _ <- many (char ' ')
  _ <- eol  -- Consume the end-of-line character
  let isEndOfLine = isNothing semicolon
  return $ Print ln (fromMaybe (ConstStr "") expr') isEndOfLine



keywordWithLine :: String -> Parser String
keywordWithLine kw = do
  _ <- lineNumber
  _ <- string kw <* space1
  return kw

keywordIf :: Parser String
keywordIf = string "IF" <* space1

keywordThen :: Parser String
keywordThen = string "THEN" <* space1


ifStmt :: Parser Stmt
ifStmt = do
  ln <- lineNumber
  _ <- keywordIf
  condition <- expr
  _ <- space1
  _ <- keywordThen

  ifBody <- manyTill stmt (lookAhead (try (keywordWithLine "ELSE") <|> try (keywordWithLine "END")))

  elseOrEnd <- try (keywordWithLine "ELSE") <|> try (keywordWithLine "END")

  elseBody <- case elseOrEnd of
    -- multiple continuous under ElSE, we need to user try
    "ELSE" ->  Just <$> manyTill stmt (try (keywordWithLine "END"))
    "END"  -> return Nothing
    _      -> fail "Expected ELSE or END keyword"

  return $ If ln condition ifBody elseBody


unknownStmt :: Parser Stmt
unknownStmt = do
  unknownToken <- takeWhile1P (Just "Unknown statement") (not . isSpace)
  fail $ "Unsupported or unknown statement: " ++ show unknownToken


