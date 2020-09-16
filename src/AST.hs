module AST where

import Data.Void

data PTerm = Var String
          | IntConst Integer
          | BoolConst Bool
          | StrConst String
          | PyList [PTerm]
          | Neg PTerm
          | Not PTerm
          | TernIf PTerm PTerm PTerm
          | CallFunc String [PTerm]
          | Call2Func PTerm [PTerm]        
          | BiOp Op PTerm PTerm
          | LambdaFunc [String] PTerm
          | CallClass String
          | StaticCall String           
          deriving (Show, Eq, Ord)

data Op = And | Or  
          | Less | Greater | Eq | NotEq
          | LessEq | GreaterEq  
          | Add | Sub | Mul
          | Pow | Mod | Fld
          | AsAdd | AsSub | AsMul
          | AsPow | AsMod
          deriving (Show, Eq, Ord)

data Stmt = Seq [Stmt]
           | Module String [Stmt]
           | Assign PTerm PTerm
           | If PTerm [Stmt]
           | Else [Stmt]
           | For String PTerm [Stmt]
           | While PTerm [Stmt]
           | Function String [String] [Stmt]
           | Return PTerm
           | Class String [Stmt]
           | SingleTerm PTerm
           deriving (Show, Eq, Ord)
