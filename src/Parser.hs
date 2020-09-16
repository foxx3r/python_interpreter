{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Parser where

import Data.Text (Text, unpack)
import Control.Applicative (empty)
import Data.Char (isAlphaNum)
import NeatInterpolation (text)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import AST

type Parser = Parsec Void String

fromMaybe :: Maybe a -> a
fromMaybe (Just m) = m
fromMaybe Nothing = error "Bad parser"

lineCmnt :: Parser ()
lineCmnt  = L.skipLineComment "//"

blockCmnt :: Parser ()
blockCmnt = L.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = L.space space1 lineCmnt blockCmnt

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineCmnt empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

bracers :: Parser a -> Parser a
bracers = between (symbol "[") (symbol "]")

quotes :: Parser a -> Parser a
quotes = between (symbol "'") (symbol "'")

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

semi :: Parser String
semi = symbol ";"

comma :: Parser String
comma = symbol ","

point :: Parser String
point = symbol "."

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = [ "if", "then", "else", "while"
      , "do", "conditional" , "true", "false"
      , "list", "[", "!", "and"
      , "or", "def", "class", "return"
      , "elif", "Call", "Void", "in"
      , "lambda", "module", "for"
      ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
  p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x
