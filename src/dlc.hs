module Main
    where

import System.Environment(getArgs)

-- import Scanner.DLCScanner -- DEBUG
import Parser.DLCParser

processFile :: [Char] -> IO ()
processFile filename = do
    code <- readFile filename
    putStrLn $ show $ run_parser code -- alexScanTokens code

main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then processFile $ head args
        else putStrLn "usage: dlc FILE"
