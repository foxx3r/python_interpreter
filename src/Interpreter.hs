{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Interpreter where

import AST
import Parser
import Text.Megaparsec
import qualified Control.Monad.Fail as Fail
import Control.Monad
import qualified Data.Map as M
import Text.Pretty.Simple (pPrint)
import Data.Bits ((.&.), (.|.))

data MyEither
    = Term PTerm
    | Statment Stmt
    deriving (Show)

newtype Context = Context {getContext :: M.Map PTerm MyEither} deriving (Show)

type Env = Either String

createContext :: Context
createContext = Context {getContext = M.empty}

getValue :: Context -> PTerm -> Env PTerm
getValue ctx name = case M.lookup name $ getContext ctx of
    Just (Term l) -> return l
    otherwise -> Left ("Missed variable " ++ (show name))

getStmt :: Context -> PTerm -> Env Stmt
getStmt ctx name = case M.lookup name $ getContext ctx of
    Just (Statment r) -> return r
    otherwise -> Left ("Missed declaration " ++ (show name))

setValue :: Context -> PTerm -> PTerm -> Env Context
setValue ctx name value = return $ Context {getContext = M.insert name (Term value) $ getContext ctx}

setStmt :: Context -> PTerm -> Stmt -> Env Context
setStmt ctx name body = return $ Context {getContext = M.insert name (Statment body) $ getContext ctx}

findFunc :: String -> [Stmt] -> Env Stmt
findFunc name (x : xs) = case x of
    fun@(Function name' _ _) | name == name' -> return fun
    cls@(Class name' _) | name == name' -> return cls
    otherwise -> findFunc name xs
findFunc name [] = fail "No body in class"
