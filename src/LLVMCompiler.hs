module LLVMCompiler where

import Control.Monad.Except ( runExceptT, MonadError(throwError), ExceptT )
import Control.Monad.State ( MonadState(put, get), StateT(runStateT) ) 
import AbsInstant ( Exp(..), Ident, Program(..), Stmt(..) )
import Data.Map ( Map, (!) )
import qualified Data.Map as Map

type LLVMState = (Map.Map Ident Int , Int, [String])
type LLVMMonad = StateT LLVMState (ExceptT String IO)

data Operation = AddOp | SubOp | MulOp | DivOp deriving Eq

data Instruction =
      PrintInt Int
    | Add Int Int Int
    | Sub Int Int Int
    | Mul Int Int Int 
    | Div Int Int Int
    | Store Int Integer

instance Show Instruction where
    show (PrintInt x)  = "call void @printInt(i32 %" ++ show x ++ ")"
    show (Add r x y)   = "%" ++ show r ++ " = " ++ "add" ++ " i32 %" ++ show x ++ ", %" ++ show y
    show (Sub r x y)   = "%" ++ show r ++ " = " ++ "sub" ++ " i32 %" ++ show x ++ ", %" ++ show y
    show (Mul r x y)   = "%" ++ show r ++ " = " ++ "mul" ++ " i32 %" ++ show x ++ ", %" ++ show y
    show (Div r x y)   = "%" ++ show r ++ " = " ++ "sdiv" ++ " i32 %" ++ show x ++ ", %" ++ show y
    show (Store r x)   = "%" ++ show r ++ " = add i32 0, " ++ show x

getRegister:: LLVMMonad Int
getRegister = do
    (names, register, result) <- get
    put (names, register + 1, result)
    return register

handleArithm :: Exp -> Exp -> Operation -> LLVMMonad Int
handleArithm e1 e2 op = do
    r1 <- handleExp e1
    r2 <- handleExp e2
    register <- getRegister
    case op of
        AddOp ->  do
            (names, register, result) <- get
            put (names, register, result ++ [show (Add register r1 r2)])
            return register
        SubOp ->  do
            (names, register, result) <- get
            put (names, register, result ++ [show (Sub register r1 r2)])
            return register
        MulOp ->  do
            (names, register, result) <- get
            put (names, register, result ++ [show (Mul register r1 r2)])
            return register
        DivOp ->  do
            (names, register, result) <- get
            put (names, register, result ++ [show (Div register r1 r2)])
            return register

handleExp :: Exp -> LLVMMonad Int
handleExp (ExpAdd e1 e2)        = handleArithm e1 e2 AddOp
handleExp (ExpSub e1 e2)        = handleArithm e1 e2 SubOp
handleExp (ExpMul e1 e2)        = handleArithm e1 e2 MulOp
handleExp (ExpDiv e1 e2)        = handleArithm e1 e2 DivOp
handleExp (ExpLit int)          = do 
    (names, register, result) <- get
    put (names, register + 1, result ++ [show (Store (register + 1) int)])
    return (register + 1)
handleExp (ExpVar ident)        = do
    (names, register, result) <- get
    if Map.member ident names
            then return $ names ! ident
            else throwError $ "Variable without value" ++ show ident

handleStmt :: Stmt -> LLVMMonad ()
handleStmt (SExp expr)          = do
    v <- handleExp expr
    (names, register, result) <- get
    put (names, register, result ++ [show (PrintInt v)])
handleStmt (SAss ident expr)    = do
    v <- handleExp expr
    (names, register, result) <- get
    put(Map.insert ident v names, register, result)

handleStmts :: [Stmt] -> LLVMMonad ()
handleStmts (x:xs) = do
    handleStmt x
    handleStmts xs
handleStmts [] = return ()

compiler :: Program -> IO (Either String ((), LLVMState))
compiler (Prog stmts) = runExceptT $ runStateT (handleStmts stmts) (Map.empty, 0, [])