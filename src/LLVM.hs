module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Process
import System.FilePath
import System.Exit

import AbsInstant ()
import ErrM ()
import ParInstant ( myLexer, pProgram )
import LLVMCompiler (compiler)

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
              writeFile (replaceExtension fName "ll") $ unlines $ llvmHeader ++ result ++ llvmFooter
              child <- runCommand $ "llvm-as " ++ replaceExtension fName "ll"
              waitForProcess child
              exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> putStr "Missing input file name" 
    fs  -> mapM_ runFile fs

llvmHeader :: [String]
llvmHeader =[
                "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
                "",
                "declare i32 @printf(i8*, ...)",
                "",
                "define void @printInt(i32 %x) {",
                "       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
                "       call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
                "       ret void",
                "}",
                "",
                "define i32 @main(i32 %argc, i8** %argv) {"
            ]

llvmFooter :: [String]
llvmFooter =[
                "  ret i32 0",
                "}"
            ]