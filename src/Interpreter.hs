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

findField :: PTerm -> [Stmt] -> Env PTerm
findField name (x : xs) = case x of
    (Assign name' term) | name == name' -> return term
    otherwise -> findField name xs
findField name [] = fail "No body in class"   

run :: String -> IO ()
run arg = pPrint $ evalProg $ fromMaybe $ parseMaybe whileParser arg

evalProg :: Stmt -> Env Context
evalProg l = evalSeq createContext l

evalSeq :: Context -> Stmt -> Env Context
evalSeq ctx (Module name value) = foldM (evalSeq) ctx value
--     helper ctx (x:xs) = do
--         ctx' <- evalSeq ctx x
--         helper ctx' xs
--     helper ctx [] = return ctx
    
evalSeq ctx (Seq value) = foldM (evalSeq) ctx value
--     helper ctx (x:xs) = do
--         ctx' <- evalStmt ctx x
--         helper ctx' xs
--     helper ctx [] = return ctx
evalSeq ctx v = evalStmt ctx v

evalStmt :: Context -> Stmt -> Env Context
evalStmt ctx (Module _ _) = fail "Can not be 2 module in 1 file"
evalStmt ctx seq@(Seq value) = evalSeq ctx seq
evalStmt ctx (Assign name term) = do
    case name of
        (BiOp Fld l (Var value)) -> do
            case l of
                (StaticCall name') -> do
                    term' <- getStmt ctx term
                    cls <- getStmt ctx (Var name')
                    case cls of
                        (Class name'' body) -> do
                            ctx' <- setStmt ctx (Var name') (Class name' (term' : body))
                            return ctx'
                        otherwise -> fail "Incorrect init"
                otherwise -> fail "Incorrect fold operation"
        otherwise -> do
            term' <- eval ctx term
            ctx' <- setValue (snd term') name (fst term')
            return ctx'
evalStmt ctx (Function name arg body) = helper ctx name arg body where
    helper ctx name arg [] = fail "Expected an indented block"
    helper ctx name arg body = do
        ctx' <- setStmt ctx (Var name) (Function name arg body) 
        return ctx'
evalStmt ctx (Class name body) = helper ctx name body where
    helper ctx name [] = fail "Expected an indented block"
    helper ctx name body = do
        ctx' <- setStmt ctx (Var name) (Class name body) 
        return ctx'
evalStmt ctx (Return value) = do
    term' <- eval ctx value
    case (fst term') of
        (LambdaFunc _ _) -> setValue (snd term') (Var "return") (fst term')
        otherwise -> setValue ctx (Var "return") (fst term')
evalStmt ctx (If cond body) = do
    cond' <- eval ctx cond
    case fst cond' of
        IntConst 0 -> setValue (snd cond') (Var "conditional") (BoolConst False)
        BoolConst False -> setValue (snd cond') (Var "conditional") (BoolConst False)
        StrConst "" -> setValue (snd cond') (Var "conditional") (BoolConst False)
        otherwise -> evalSeq (snd cond') (Seq body)
evalStmt ctx (Else body) = do
    case getValue ctx (Var "conditional") of
        Left _ -> return ctx
        otherwise -> evalSeq ctx (Seq body)
evalStmt ctx (While cond body) = do
    cond' <- eval ctx cond
    case fst cond' of
        IntConst 0 -> return ctx
        BoolConst False -> return ctx
        StrConst "" -> return ctx
        otherwise -> do 
            ctx' <- evalSeq ctx (Seq body)
            evalStmt ctx' (While cond body)
evalStmt ctx (For exp cont body) = do
    cont' <- eval ctx cont
    case fst cont' of
        StrConst (x:xs) -> do
            ctx' <- setValue ctx (Var exp) (StrConst [x])
            ctx'' <- evalSeq ctx' (Seq body)
            evalStmt ctx'' (For exp (StrConst xs) body)
        PyList (l:ls) -> do
            ctx' <- setValue ctx (Var exp) l
            ctx'' <- evalSeq ctx' (Seq body)
            evalStmt ctx'' (For exp (PyList ls) body)
        StrConst [] -> return ctx
        PyList [] -> return ctx
        otherwise -> fail "Incorrect container"
evalStmt ctx (SingleTerm term) = case term of
    (BiOp AsAdd l _) -> do
        case l of
            l'@(Var _) -> do 
                term' <- eval ctx term
                ctx' <- setValue (snd term') l' (fst term')
                return ctx'
            otherwise -> fail "Can't assign to literal"
    (BiOp AsSub l _) -> do
        case l of
            l'@(Var _) -> do 
                term' <- eval ctx term
                ctx' <- setValue (snd term') l' (fst term')
                return ctx'
            otherwise -> fail "Can't assign to literal"
    (BiOp AsMul l _) -> do
        case l of
            l'@(Var _) -> do 
                term' <- eval ctx term
                ctx' <- setValue (snd term') l' (fst term')
                return ctx'
            otherwise -> fail "Can't assign to literal"
    (BiOp AsMod l _) -> do
        case l of
            l'@(Var _) -> do 
                term' <- eval ctx term
                ctx' <- setValue (snd term') l' (fst term')
                return ctx'
            otherwise -> fail "Can't assign to literal"
    (BiOp AsPow l _) -> do
        case l of
            l'@(Var _) -> do 
                term' <- eval ctx term
                ctx' <- setValue (snd term') l' (fst term')
                return ctx'
            otherwise -> fail "Can't assign to literal"
    otherwise -> do
        term' <- eval ctx term
        ctx' <- setValue (snd term') (Var "temp") (fst term')
        return ctx'

evalArgs :: Context -> [String] -> [PTerm] -> Env Context
evalArgs ctx (a:as) (p:ps)= do
    term' <- eval ctx p
    ctx' <- setValue (snd term') (Var a) (fst term') 
    evalArgs ctx' as ps 
evalArgs ctx [] [] = return ctx
evalArgs _ _ _ = fail "Different numbers of argument and parameters" 
    
eval :: Context -> PTerm -> Env (PTerm, Context)
eval ctx term = case term of
    x@(Var _) -> do
        term' <- getValue ctx x
        return (term', ctx)
    int@(IntConst _) -> return (int, ctx)
    bl@(BoolConst _) -> return (bl, ctx)
    str@(StrConst _) -> return (str, ctx)
    (BiOp Add l r) -> do 
        l' <- eval ctx l 
        case fst l' of
          (StrConst l'') -> strAdd ctx (++) l'' r
          (PyList l'') -> listAdd ctx (++) l'' r 
          otherwise -> intOp ctx (+) l r
    (BiOp Mul l r) -> do
        l' <- eval ctx l
        case fst l' of 
            (StrConst l'') -> strMul ctx l'' r
            (PyList l'') -> listMul ctx (fst l') r 
            (IntConst l'') -> do
                r' <- eval ctx r
                case fst r' of
                    (PyList r'') -> listMul ctx (fst l') (fst r')
                    otherwise -> intOp ctx (*) l r
            otherwise -> fail "Incorrect left operand"                
    (BiOp Sub l r) -> intOp ctx (-) l r
    (BiOp And l r) -> intOp ctx (.&.) l r
    (BiOp Or l r) -> intOp ctx (.|.) l r
    (BiOp Mod l r) -> intOp ctx (mod) l r
    (BiOp Less l r) -> binOp ctx (<) l r
    (BiOp Greater l r) -> binOp ctx (>) l r
    (BiOp Eq l r) -> binOp ctx (==) l r
    (BiOp NotEq l r) -> binOp ctx (/=) l r
    (BiOp GreaterEq l r) -> binOp ctx (>=) l r
    (BiOp LessEq l r) -> binOp ctx (<=) l r
    (BiOp Pow l r) -> intOp ctx (^) l r
    (BiOp AsAdd l r) -> unOp ctx (+) l r
    (BiOp AsSub l r) -> unOp ctx (-) l r
    (BiOp AsMul l r) -> unOp ctx (*) l r
    (BiOp AsPow l r) -> unOp ctx (^) l r
    (BiOp AsMod l r) -> unOp ctx (mod) l r    
    (Neg x) -> case eval ctx x of
        Right ((IntConst value), ctx) -> return $ (IntConst (-value), ctx)
        otherwise -> fail "Incorrect type"
    (Not x) -> case eval ctx x of
        Right ((BoolConst value), ctx) -> return $ (BoolConst (not value), ctx)
        otherwise -> fail "Incorrect type"
    list@(PyList _) -> return (list, ctx) 
    (CallFunc name arg) -> do 
        term' <- callFun ctx (Var name) arg
        case fst term' of
            LambdaFunc _ _ -> return term'
            otherwise -> return (fst term', ctx)
        
    (Call2Func name arg) -> case name of
        (CallFunc name' arg') -> do 
            body <- callFun ctx (Var name') arg'
            term' <- callFun (snd body) (fst body) arg
            return term'
        otherwise -> fail "Incorrect lambda call"           
    labd@(LambdaFunc arg body) -> return (labd, ctx)
    cls@(CallClass name) -> return (cls, ctx)
    statCall@(StaticCall name) -> return (statCall, ctx)
    (BiOp Fld l r) -> fldOp ctx l r
    (TernIf cond ifValue elseValue) -> do
        cond' <- eval ctx cond
        case fst cond' of
            IntConst 0 -> eval (snd cond') elseValue
            BoolConst False -> eval (snd cond') elseValue
            StrConst "" -> eval (snd cond') elseValue
            otherwise -> eval (snd cond') ifValue
