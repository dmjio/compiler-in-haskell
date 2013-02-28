module Main
    where

import System.Environment(getArgs)
import Scanner.DLCScanner(alexScanTokens, token_print)
import Parser.DLCParser(run_parser)

import System.Directory(doesDirectoryExist, doesFileExist, getModificationTime, createDirectory)
import System.FilePath.Posix(joinPath)

import System.FilePath.Glob(compile, globDir)

import DLC.ASTTransformer
import DLC.PrettyPrinter
import DLC.Job
import DLC.CompileDataObject

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

-- return path of all *.dl files under the first level of the given directories.
-- e.g., getDLSourceInDir ["src/Runtime"] ===> ["src/Runtime/__DL_Array.dl", "...Object.dl"]
--       (files are given in full absolute path.)
-- FIXME: globDir will not expand the user directory symbol, "~".
getDLSourceInDir :: [FilePath] -> IO ([FilePath])
getDLSourceInDir fpList =
    mapM (\fp -> do {(r,_) <- globDir [compile "*.dl"] fp; return $ concat r}) fpList >>=
    (return . concat)

buildDir :: String -> IO ()
buildDir dirName =
    let
        mainPath = joinPath [dirName, "main.dl"]
        buildPath = joinPath [dirName, "build"]
    in do
        fList <- getDLSourceInDir [dirName, "src/Runtime"]
        -- putStrLn $ show fList -- DEBUG: list all files
        pRootList <- mapM (\fp -> (readFile fp >>= (return . run_parser))) fList >>=
                     (return . concat)
        -- putStrLn $ show pRootList -- DEBUG: print the raw data structure
        -- 
        -- DEBUG: pretty print
        -- case transformParserAST pRootList of
        --     (Ok tr) -> prettyPrintTransResult 0 tr
        --     (ErrorLog eLog) -> putStrLn ("error!!! " ++ eLog)
        --  
        -- DEBUG: show CDO
        case transformParserAST pRootList >>= genCDO of
            (Ok cdo) -> prettyPrintCDO cdo
            (ErrorLog e) -> putStrLn ("error!!! " ++ e)

        -- -- TODO: parse all .dl files in dirName and Runtime;
        -- --       save them in a list, and use DLTypeChecking
        -- --       to get the huge final assembly file.
        -- dirExists <- doesDirectoryExist buildPath
        -- if not dirExists then
        --     createDirectory buildPath
        -- else
        --     return ()

buildDirLauncher :: IO ()
buildDirLauncher = do
    args <- getArgs
    if length args /= 1
    then pUsage
    else do
        dirExists <- doesDirectoryExist $ head args
        dlFList <- getDLSourceInDir [head args] -- we need at least one .dl file
        if not (dirExists && length dlFList > 0) then
            pUsage
        else
            buildDir $ head args

main :: IO ()
-- main = testScanner
-- main = testParser
main = buildDirLauncher
