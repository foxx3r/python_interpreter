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
