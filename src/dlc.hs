module Main
    where

import Scanner.DLCScanner(alexScanTokens, token_print)
import Data.Map

main :: IO ()
main = do
    code <- readFile "src/Runtime/Object.dl"
    token_print $ alexScanTokens code
    -- args <- getArgs
    -- if length args == 1
    --     then 
