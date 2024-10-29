module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Process
import System.FilePath
import System.Exit

import AbsInstant ()
import ErrM ()
import ParInstant ( myLexer, pProgram )
import JVMCompiler (compiler)

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
            Right (_, (names, register, result)) -> do
              writeFile (replaceExtension fName "j") $ unlines $ jvmHeader (takeBaseName fName) ++ jvmMain ++ result ++ jvmFooter
              child <- runCommand $ "java -jar ../lib/jasmin.jar -d " ++ (takeDirectory fName) ++ 
                        " " ++ (replaceExtension fName "j")
              waitForProcess child
              exitSuccess

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
jvmMain :: [String]
jvmMain = [
        ".method public static main([Ljava/lang/String;)V",
        ".limit locals " ++ "1024",
        ".limit stack " ++ "1024"
    ]

jvmFooter :: [String]
jvmFooter = [
        "  return",
        ".end method"
    ]

