module Main
    where

import System.Environment(getArgs)
import Scanner.DLCScanner(alexScanTokens, token_print)
import Parser.DLCParser(run_parser)

import System.Directory(doesDirectoryExist, doesFileExist, getModificationTime)
import System.FilePath.Posix(joinPath)

testScanner :: IO ()
testScanner = getArgs >>= (return . head) >>= readFile >>= (token_print . alexScanTokens)

testParser :: IO ()
testParser = getArgs >>= (return . head) >>= readFile >>= (putStrLn . show . run_parser)

pUsage :: IO ()
pUsage = putStrLn "Usage: dlc DIR"

-- is f1 newer than f2?
fNewerThan :: FilePath -> FilePath -> IO Bool
fNewerThan f1 f2 = do
    f1_exists <- doesFileExist f1
    f2_exists <- doesFileExist f2
    -- f1: N/A, f2: N/A => NO
    -- f1: N/A, f2: yes => NO
    -- f1: yes, f2: N/A => YES
    -- f1: yes, f2: yes => t1 < t2
    if (not f1_exists) then
        return False
    else if (not f2_exists) then
        return True
    else do
        f1_mod_t <- getModificationTime f1
        f2_mod_t <- getModificationTime f2
        return (f1_mod_t < f2_mod_t)

buildDir :: String -> IO ()
buildDir dirName =
    let
        mainPath = joinPath [dirName, "main.dl"]
        buildPath = joinPath [dirName, "build"]
    in do
        -- TODO: parse all .dl files in dirName and Runtime;
        --       save them in a list, and use DLTypeChecking
        --       to get the huge final assembly file.
        dirExists <- doesDirectoryExist buildPath
        putStrLn buildPath

buildDirLauncher :: IO ()
buildDirLauncher = do
    args <- getArgs
    if length args /= 1
    then pUsage
    else do
        dirExists <- doesDirectoryExist $ head args
        mainExists <- doesFileExist $ joinPath [(head args), "main.dl"]
        if not (dirExists && mainExists) then
            pUsage
        else
            buildDir $ head args

main :: IO ()
-- main = testScanner
-- main = testParser
main = buildDirLauncher
