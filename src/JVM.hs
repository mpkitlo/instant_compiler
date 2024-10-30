{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Process ( runCommand, waitForProcess )
import System.FilePath ( replaceExtension, takeBaseName, takeDirectory )
import System.Exit ( exitSuccess )

import AbsInstant ()
import ErrM ()
import ParInstant ( myLexer, pProgram )
import JVMCompiler (compiler, Instruction (PushInt, Print, Store, Load, Arithm))

runFile :: String -> IO ()
runFile fName = readFile fName >>= run fName

run :: FilePath -> String -> IO ()
run fName code =
    case pProgram $ myLexer code of
        Left err -> hPutStrLn stderr err
        Right prog -> do 
          llvmCode <- compiler prog
          case llvmCode of
            Left err -> hPutStrLn stderr err
            Right (instructions, (names, register, result)) -> do
              writeFile (replaceExtension fName "j") $ unlines $ jvmHeader (takeBaseName fName) ++ 
                jvmMain (show (register+1)) (show (calculateStack instructions 0 0 + 1)) ++ result ++ jvmFooter
              process <- runCommand $ "java -jar lib/jasmin.jar -d " ++ takeDirectory fName ++ " " ++ replaceExtension fName "j"
              waitForProcess process
              exitSuccess

calculateStack :: [Instruction] -> Int -> Int -> Int
calculateStack [] stackSize maxi = maxi
calculateStack (x:xs) stackSize maxi =
  case x of
    (PushInt  _) -> calculateStack xs (stackSize+1) (max maxi (stackSize+1))
    (Print  _) -> calculateStack xs (stackSize-1) maxi
    (Store  _) -> calculateStack xs (stackSize-1) maxi
    (Load  _) -> calculateStack xs (stackSize+1) (max maxi (stackSize+1))
    Arithm -> calculateStack xs (stackSize-1) maxi

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> putStr "Missing input file name" 
    fs  -> mapM_ runFile fs

jvmHeader :: String -> [String]
jvmHeader className = [
        ".class public " ++ className,
        ".super java/lang/Object",
        ".method public <init>()V",
        "  aload_0",
        "  invokespecial java/lang/Object/<init>()V",
        "  return",
        ".end method",
        ""
    ]
jvmMain :: String -> String -> [String]
jvmMain maxLocals maxStack = [
        ".method public static main([Ljava/lang/String;)V",
        ".limit locals " ++ maxLocals,
        ".limit stack " ++ maxStack
    ]

jvmFooter :: [String]
jvmFooter = [
        "  return",
        ".end method"
    ]

