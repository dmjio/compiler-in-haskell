module Main
    where

import System.Environment(getArgs)
import Scanner.DLCScanner(alexScanTokens, token_print)
import Data.Map

testScanner :: IO ()
testScanner = getArgs >>= (\(x:_) -> return x) >>= readFile >>= (token_print . alexScanTokens)

main :: IO ()
main = testScanner
