module Main
    where

import System.Environment(getArgs)
import Scanner.DLCScanner(alexScanTokens, token_print)
import Parser.DLCParser(run_parser)
import Data.Map

testScanner :: IO ()
testScanner = getArgs >>= (\(x:_) -> return x) >>= readFile >>= (token_print . alexScanTokens)

testParser :: IO ()
testParser = getArgs >>= (return . head) >>= readFile >>= (putStrLn . show . run_parser)

main :: IO ()
-- main = testScanner
main = testParser
