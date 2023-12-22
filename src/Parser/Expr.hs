{-# LANGUAGE OverloadedStrings #-}

module Parser.Expr
  ( expr,
    Expr (..),
    identifier,
    varExpr,
    constExpr,
    stringLiteral,
    -- test
    unaryExpr,
    number,
    stringExpr,
    arithExpr,
    term
  )
where

import Control.Monad.Combinators
import Parser.Common (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space,
    space1,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L

-- import Text.Megaparsec.Debug (dbg)
-- import Debug.Trace (traceShow, traceShowId)

-- Parses an integer
number :: Parser Int
number = L.decimal -- <* space

-- Parses specific keyword (e.g. variable name)
identifier :: Parser String
identifier = (:) <$> letterChar <*> many alphaNumChar

-- Expressions

data Expr
  = Var String
  | ConstInt Int
  | ConstStr String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Eq Expr Expr -- =
  | Negate Expr
  | And Expr Expr
  deriving (Show, Eq)

unaryExpr :: Parser Expr
unaryExpr = do
  _ <- char '-'
  expr' <- constExpr <|> varExpr -- Use the appropriate sub-parser here
  return $ Negate expr'

constExpr :: Parser Expr
constExpr = ConstInt <$> number

varExpr :: Parser Expr
varExpr = Var <$> identifier

stringExpr :: Parser Expr
stringExpr = ConstStr <$> stringLiteral

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

factor :: Parser Expr
factor = try unaryExpr <|> try constExpr <|> try stringExpr <|> varExpr -- <|> parenthesizedExpr

operatorParser :: Parser (Expr -> Expr -> Expr)
operatorParser = do
  space -- Consume leading spaces
  op <- Mult <$ char '*' <|> Div <$ char '/' -- Parse the operator
  space -- Consume trailing spaces
  return op

arithOperatorParser :: Parser (Expr -> Expr -> Expr)
arithOperatorParser = do
  space -- Consume leading spaces
  op <- Add <$ char '+' <|> Sub <$ char '-' -- Parse the operator
  space -- Consume trailing spaces
  return op

term :: Parser Expr
term = do
  e1 <- factor
  more <- many $ try $ (,) <$> operatorParser <*> factor
  return $ foldl (\acc (op, e) -> op acc e) e1 more

arithExpr :: Parser Expr
arithExpr = do
  e1 <- term
  more <- many $ try $ (,) <$> arithOperatorParser <*> term
  return $ foldl (\acc (op, e) -> op acc e) e1 more

keywordAnd :: Parser String
keywordAnd = space1 *> string "AND" <* space1

andExpr :: Parser Expr
andExpr = do
  left <- primaryExpr
  _ <- keywordAnd
  And left <$> primaryExpr

eqExpr :: Parser Expr
eqExpr = do
  e1 <- varExpr <|> constExpr
  _ <- optional space
  _ <- string "=" <* space
  e2 <- varExpr <|> constExpr
  return $ Eq e1 e2

primaryExpr :: Parser Expr
primaryExpr = try eqExpr <|> arithExpr

expr :: Parser Expr
expr = try andExpr <|> primaryExpr
