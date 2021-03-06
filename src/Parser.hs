{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
rws = ["if", "then", "else", "while"
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

classIdentifier :: Parser String
classIdentifier = (lexeme . try) (p >>= check)
    where
        p = (:) <$> (upperChar <|> char '_') <*> many (alphaNumChar <|> char '_')
        check x = if x `elem` rws
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

fWord :: Parser String
fWord = (lexeme . try) p 
    where
        p = (:) <$> (alphaNumChar <|> char '_') <*> many (alphaNumChar <|> char '_')

fieldIndent :: Parser String
fieldIndent = (lexeme . try) $ do
    first <- identifier
    void (symbol ".")
    second <- identifier
    return (first ++ "." ++ second)

strIndent :: Parser String
strIndent = (lexeme . try) $ do
    name <- quotes identifier
    return name

dynMetIndent :: Parser String
dynMetIndent = (lexeme . try) $ do
    first <- fieldIndent
    second <- (try $ bracers strIndent) <|> (try $ parens strIndent)  
    return (first ++ second)

check' :: PTerm -> Bool
check' (Var x) = False
check' _  = True

moduleParser :: Parser Stmt
moduleParser = L.nonIndented scn (L.indentBlock scn p)
    where
        p = do
            rword "module"
            name <- identifier
            return (L.IndentSome (Just (mkPos 5)) (return . (Module name)) stmt)

whileParser :: Parser Stmt
whileParser = between sc eof stmt

stmt :: Parser Stmt
stmt = f <$> sepBy1 stmt' semi
    where
        f [x] = x
        f xs  = Seq xs

stmt' :: Parser Stmt
stmt' = try moduleParser
    <|> try whileStmt
    <|> try forStmt
    <|> try assignclsExpr
    <|> try assignTerm 
    <|> try ifStmt
    <|> try (parens stmt)
    <|> try (bracers stmt) 
    <|> try defFun
    <|> try defClass
    <|> try return'
    <|> try elseStmt
    <|> try (SingleTerm <$> pyExpr)

defFun :: Parser Stmt
defFun = L.indentBlock scn p
    where
        p = do
            rword "def"
            name <- identifier
            arg <- parens (sepBy identifier comma)
            symbol ":"
            return (L.IndentSome Nothing (return . (Function name arg)) stmt)   

callFunc :: Parser PTerm
callFunc = do
    name1 <- try dynMetIndent <|> identifier
    arg <- parens (sepBy pyExpr comma)
    return (CallFunc name1 arg)

call2Func :: Parser PTerm
call2Func = do
    name1 <- identifier
    arg <- parens (sepBy pyExpr comma)
    arg1 <- parens (sepBy1 pyExpr comma)
    return (Call2Func (CallFunc name1 arg) arg1)

lambFunc :: Parser PTerm
lambFunc = do
    rword "lambda"
    arg <- sepBy identifier comma
    void (symbol ":")
    body <- try ternIfTerm <|> pyExpr
    return (LambdaFunc arg body) 

return' :: Parser Stmt
return' = do
    rword "return"
    expr <- pyExpr
    return (Return expr)

listPars :: Parser PTerm
listPars = do
    arg <- bracers (sepBy pyExpr comma)
    return (PyList arg)

defClass :: Parser Stmt
defClass = L.indentBlock scn p
    where
        p = do
            rword "class"
            name <- identifier
            void (symbol ":")
            return (L.IndentMany Nothing (return . (Class name)) stmt)

callStatClass :: Parser PTerm
callStatClass = do
    name <- classIdentifier
    return (StaticCall name)

callClass :: Parser PTerm
callClass = do
    name <- classIdentifier
    void (symbol "()")
    return (CallClass name)

ifStmt :: Parser Stmt
ifStmt = L.indentBlock scn p
    where
        p = do
            rword "if"
            cond <- pyExpr
            void (symbol ":")
            return (L.IndentMany Nothing (return . (If cond)) stmt)

ternIfTerm :: Parser PTerm
ternIfTerm = do
    term <- pyExpr
    rword "if"
    cond <- pyExpr
    rword "else"
    term' <- pyExpr
    return (TernIf cond term term')


elseStmt :: Parser Stmt
elseStmt = L.indentBlock scn p
    where
        p = do
            rword "else"
            void (symbol ":")
            return (L.IndentMany Nothing (return . (Else)) stmt)     

forStmt :: Parser Stmt
forStmt = L.indentBlock scn p
    where
        p = do
            rword "for"
            exp <- identifier
            rword "in"
            cont <- pyExpr
            void (symbol ":")
            return (L.IndentMany Nothing (return . (For exp cont)) stmt)

whileStmt :: Parser Stmt
whileStmt = L.indentBlock scn p
    where
        p = do
            rword "while"
            cond <- pyExpr
            void (symbol ":")
            return (L.IndentMany Nothing (return . (While cond)) stmt)  

assignTerm :: Parser Stmt
assignTerm = do
    var <- identifier
    void <- (symbol "=")
    expr <- try lambFunc <|> try ternIfTerm <|> pyExpr 
    return (Assign (Var var) expr)

assignclsExpr :: Parser Stmt
assignclsExpr = do
    var <- clsExpr
    void <- (symbol "=")
    expr <- try lambFunc <|> try ternIfTerm <|> pyExpr
    return (Assign var expr)

pyExpr :: Parser PTerm
pyExpr = try (makeExprParser term eOperators) <|> term

clsExpr :: Parser PTerm
clsExpr = try (makeExprParser term fldOperator) <|> term

fldOperator :: [[Operator Parser PTerm]]
fldOperator = [[ InfixL (BiOp Fld <$ string ".") ]]

eOperators :: [[Operator Parser PTerm]]
eOperators =
  [
    [Prefix (Not <$ rword  "not")
      , Prefix (Neg <$ symbol "-")]
    ,
    [InfixL (BiOp Fld <$ string ".")]
    ,
    map InfixL
    [BiOp AsMod <$ symbol "%="
      , BiOp Mod <$ symbol "%"
      , BiOp AsPow <$ symbol "**="
      , BiOp Pow <$ symbol "**"
    ]
    ,
    map InfixL
    [BiOp AsMul <$ symbol "*="
      , BiOp Mul <$ symbol "*"
    ]
    ,
    map InfixL
    [BiOp AsAdd <$ symbol "+="
      , BiOp AsSub <$ symbol "-="
      , BiOp Add <$ symbol "+"
      , BiOp Sub <$ symbol "-"
      ]
    ,
    map InfixN
    [BiOp Eq <$ symbol "=="
      , BiOp NotEq <$ symbol "!="
      , BiOp LessEq <$ symbol "<="
      , BiOp Less <$ symbol "<"
      , BiOp GreaterEq  <$ symbol ">="
      , BiOp Greater <$ symbol ">"
    ]
    ,
    [InfixL (BiOp And <$ rword "and")
      , InfixL (BiOp Or  <$ rword "or")]
  ]

term :: Parser PTerm
term = parens pyExpr
    <|> try callClass
    <|> try callStatClass
    <|> try call2Func
    <|> try callFunc
    <|> try (bracers pyExpr)
    <|> try listPars
    <|> try lambFunc
    <|> try (Var <$> identifier)
    <|> try (StrConst <$> strIndent)
    <|> try (IntConst <$> integer)
    <|> (BoolConst True <$ rword "true")
    <|> (BoolConst False <$ rword "false")
