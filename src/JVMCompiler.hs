module JVMCompiler where

import Control.Monad.Except
import Control.Monad.State 
import Data.Set as Set
import AbsInstant
import Data.Map (Map, (!))
import qualified Data.Map as Map

type JVMState = (Map.Map Ident Int, Int, [String])
type JVMMonad = StateT JVMState (ExceptT String IO)

data Operation = AddOp | SubOp | MulOp | DivOp deriving Eq

data Instruction =
        PushInt Integer
    |   Print [String]
    |   Store Int
    |   Load Int
    |   Arithm

instance Show Instruction where
    show (PushInt int)
        | int < 6 = "iconst_" ++ show int
        | int < 128 = "bipush " ++ show int
        | int < 32768 = "sipush " ++ show int
        | otherwise = "ldc " ++ show int
    show (Print code) = "getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++ concat code ++ "\ninvokevirtual java/io/PrintStream/println(I)V"
    show (Store int)  
        | int <= 3 = "istore_" ++ show int
        | otherwise = "istore " ++ show int
    show (Load int) 
        | int <= 3 = "iload_" ++ show int
        | otherwise = "iload " ++ show int
    
getRegister:: JVMMonad Int
getRegister = do
    (names, register, result) <- get
    put (names, register + 1, result)
    return register
        
handleArithm :: Exp -> Exp -> Operation -> JVMMonad (Int, [String], [Instruction])
handleArithm e1 e2 op = do
    (r1, [str1], instructions)  <- handleExp e1
    (r2, [str2], instructions2) <- handleExp e2
    register <- getRegister
    case op of
        AddOp ->  do
            (names, register, result) <- get
            put (names, register, result)
            return (register, [str1 ++ "\n" ++ str2 ++ "\n" ++ "iadd"], instructions ++ instructions2 ++ [Arithm])
        SubOp ->  do
            (names, register, result) <- get
            put (names, register,  result)
            return (register, [str1 ++ "\n" ++ str2 ++ "\n" ++ "isub"], instructions ++ instructions2 ++ [Arithm])
        MulOp ->  do
            (names, register, result) <- get
            put (names, register, result)
            return (register, [str1 ++ "\n" ++ str2 ++ "\n" ++ "imul"], instructions ++ instructions2 ++ [Arithm])
        DivOp ->  do
            (names, register, result) <- get
            put (names, register, result)
            return (register, [str1 ++ "\n" ++ str2 ++ "\n" ++ "idiv"], instructions ++ instructions2 ++ [Arithm])

handleExp :: Exp -> JVMMonad (Int, [String], [Instruction])
handleExp (ExpAdd e1 e2)        = handleArithm e1 e2 AddOp
handleExp (ExpSub e1 e2)        = handleArithm e1 e2 SubOp
handleExp (ExpMul e1 e2)        = handleArithm e1 e2 MulOp
handleExp (ExpDiv e1 e2)        = handleArithm e1 e2 DivOp
handleExp (ExpLit int)          = do
    (names, register, result) <- get
    put (names, register, result)
    return (register, [show (PushInt int)], [PushInt int])
handleExp (ExpVar ident)        = do
    (names, register, result) <- get
    if Map.member ident names
            then do 
                put (names, register, result)
                return (names ! ident, [show (Load (names ! ident))], [Load (names ! ident)])
            else throwError $ "Variable without value" ++ show ident

handleStmt :: Stmt -> JVMMonad [Instruction]
handleStmt (SExp expr)         = do
    (names, register,  result) <- get
    (r, [str], instructions) <- handleExp expr 
    put (names, register, result ++ [show $ Print [str]])
    return (instructions++[Print [str]])
handleStmt (SAss ident expr)   = do
    (names, register, result) <- get
    if Map.member ident names
        then do
            (r2, [str2], instructions) <- handleExp expr
            put(names, register, result ++ [str2] ++ [show (Store (names ! ident))])
            return (instructions++[Store (names ! ident)])
        else do
            r <- getRegister
            (_, [str], instructions) <- handleExp expr
            put(Map.insert ident r names, r+1, result ++ [str] ++ [show (Store r)])
            return (instructions++[Store r])

handleStmts :: [Stmt] -> [Instruction] -> JVMMonad [Instruction]
handleStmts (x:xs) acc = do
    res <- handleStmt x
    handleStmts xs (acc++res)
handleStmts [] acc = return acc

compiler :: Program -> IO (Either String ([Instruction], JVMState))
compiler (Prog stmts) = runExceptT $ runStateT (handleStmts stmts []) (Map.empty, 0, [])