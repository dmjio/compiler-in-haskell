module Main
    where

import System.Environment(getArgs)
-- import System.FilePath.Posix(takeDirectory, takeBaseName)
-- import System.Posix.Directory(changeWorkingDirectory)
-- import System.Directory(createDirectory, doesDirectoryExist)
import System.FilePath.Posix
import System.Posix.Directory
import System.Directory
import System.IO (IOMode(..), hClose, openFile, hPutStr)

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

compileTo :: FilePath -> [Func] -> IO()
compileTo path funcList =
    do
        f <- openFile path WriteMode
        hPutStr f asHeader
        mapM_ (\func -> compileFuncTo f func) funcList
        hClose f
    where
        asHeader = ".data\n\n.text\n"

processFile :: FilePath -> IO ()
processFile filePath =
    let
        workDir = takeDirectory filePath
        filename = takeFileName filePath
        cMainPath = "build/main.c"
        asPath = "build/" ++ filename ++ ".s"
    in do
        changeWorkingDirectory workDir
        wd <- getWorkingDirectory -- chdir doesn't work without it...
        buildDirExists <- doesDirectoryExist "build"
        if not buildDirExists
            then System.Directory.createDirectory "build"
            else return ()
        writeCMain cMainPath
        code <- readFile filename
        compileTo asPath $ run_parser code

    -- putStrLn $ show $ run_parser code -- alexScanTokens code

main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then processFile $ head args
        else putStrLn "usage: dlc FILE"
