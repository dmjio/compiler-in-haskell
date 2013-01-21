module Main
    where

import System.Environment(getArgs)
import System.FilePath.Posix
import System.Posix.Directory
import System.Directory
import System.IO (IOMode(..), hClose, openFile, hPutStr, Handle)

import Data.Map

import Debug.Trace(trace)

-- import Scanner.DLCScanner -- DEBUG
import Parser.DLCParser

import Compile(compileFuncTo)

writeCMain :: FilePath -> IO ()
writeCMain path =
    do
        f <- openFile path WriteMode
        hPutStr f s
        hClose f
    where
        s = unlines [
                "#include <stdio.h>",
                "",
                "extern int dlc_main(void);",
                "",
                "int main(int argc, char *argv[]) {",
                "    return dlc_main();",
                "}"]

writeMakefile :: FilePath -> String -> IO ()
writeMakefile path fname =
    do
        f <- openFile path WriteMode
        hPutStr f s
        hClose f
    where
        dlas = fname ++ ".s"
        dlo = dlas ++ ".o"
        s = unlines [
                "a.out: main.o " ++ dlo,
                "\tcc -o a.out main.o " ++ dlo,
                "",
                "main.o: main.c",
                "\tcc -c main.c -o main.o",
                "",
                dlo ++ ": " ++ dlas,
                "\tas -o " ++ dlo ++ " " ++ dlas,
                ""]

compileTo :: FilePath -> [Func] -> IO ()
compileTo path funcList =
    do
        f <- openFile path WriteMode
        hPutStr f asHeader
        -- putStrLn $ show $ sMap -- DEBUG
        mapM_ (\func -> compileFuncTo f func sMap) funcList
        hClose f
    where
        asHeader = ".data\n\n.text\n"
        sMap = getFuncSignMap funcList

getFuncName :: Func -> String
getFuncName (_, fname, _, _) = fname

getFuncRetType :: Func -> Type
getFuncRetType (t, _, _, _) = t

getFuncParamTypeList :: Func -> [Type]
getFuncParamTypeList (x, y, ((t, _):ts), z) =
    t:(getFuncParamTypeList (x, y, ts, z))
getFuncParamTypeList (_, _, [], _) = []

getFuncSignMap :: [Func] -> Map String (Type, [Type])
getFuncSignMap [] = empty
getFuncSignMap (func:fs) = sMap
    where
        funcName = getFuncName func
        funcRetType = getFuncRetType func
        funcParamTypeList = getFuncParamTypeList func
        sign = (funcRetType, funcParamTypeList)
        sMap = insert funcName sign (getFuncSignMap fs)

processFile :: FilePath -> IO ()
processFile filePath =
    let
        workDir = takeDirectory filePath
        filename = takeFileName filePath
        cMainPath = "build/main.c"
        makefilePath = "build/Makefile"
        asPath = "build/" ++ filename ++ ".s"
    in do
        changeWorkingDirectory workDir
        wd <- getWorkingDirectory -- chdir doesn't work without it...
        buildDirExists <- doesDirectoryExist "build"
        if not buildDirExists
            then System.Directory.createDirectory "build"
            else return ()
        writeCMain cMainPath
        writeMakefile makefilePath filename
        code <- readFile filename
        compileTo asPath $ run_parser code

    -- putStrLn $ show $ run_parser code -- alexScanTokens code

main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then processFile $ head args
        else putStrLn "usage: dlc FILE"
