{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude
    ( String
    , Int
    , IO
    , Show
    , return
    , putStrLn
    , getLine
    , (<$>)
    , (<*>)
    , ($)
    )
import Control.Monad (void, forever)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

newtype Type = Type { extractNum :: Int }

data LExpr
  = Lambda String LExpr
  | App LExpr LExpr
  | Const String
  deriving Show

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt =  L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = (:) <$> letterChar <*> many alphaNumChar

whileParser :: Parser LExpr
whileParser = between sc eof lexpr

lexpr :: Parser LExpr
lexpr = lambda <|> app <|> const

lambda :: Parser LExpr
lambda = do
  string "\\"
  var <- identifier
  string "."
  lexpr' <- lexpr
  return (Lambda var lexpr')

app :: Parser LExpr
app = parens $ do
  exp1 <- lexpr
  sc
  exp2 <- lexpr
  return (App exp1 exp2)

const :: Parser LExpr
const = do
  var <- identifier
  return (Const var)

main :: IO ()
main = forever $ do
  str <- getLine
  parseTest' lexpr str
