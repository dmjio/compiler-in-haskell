module Compile(compileFuncTo)
    where

import System.IO
import Parser.DLCParser

import Data.Map

-- current stack usage:
-- [(String, Type)]

registerTable = [
    ["%rax", "%eax",  "%ax",  "%al"],
    ["%rdi", "%edi",  "%di", "%dil"],
    ["%rsi", "%esi",  "%si", "%sil"],
    ["%rdx", "%edx",  "%dx",  "%dl"],
    ["%rcx", "%ecx",  "%cx",  "%cl"],
    [ "%r8", "%r8d", "%r8w", "%r8b"],
    [ "%r9", "%r9d", "%r9w", "%r9b"]]

-- FIXME: number of params is restricted to no more than 6 for now
compileFuncTo :: Handle -> Func -> Map String (Type, [(Type)]) -> IO ()
compileFuncTo f (retType, funcNameOrig, paramList, funcBody) sMap =
    let
        fname = convDLCFuncName funcNameOrig
--         (initialStackInfo, pushParamInstrs) =
--             pushParamsIntoStack paramList (tail registerTable)
    in do
        hPutStr f (".globl " ++ fname ++ "\n")
        hPutStr f (fname ++ ":\n")
        -- putStrLn $ show sMap -- DEBUG

convDLCFuncName :: String -> String
convDLCFuncName f = if f == "main" then "_dlc_main" else "__dlc_f_" ++ f

-- -- return *initial stack information* and *assembly instructions used*
-- -- to push the params into the stack.
-- pushParamsIntoStack :: [(Type, String)] -> [[String]] -> ([String, Type], [String])
-- pushParamsIntoStack [] _ = ([], [])
-- pushParamsIntoStack (t, v):xs r:rs =
--     let
--         (info, instrList) = pushParamsIntoStack xs
--         info_new = (v, t):info
--         -- FIXME: assuming only ints here...
--         instr = "pushl " ++ r!!1
--     in
--         (info_new, instr:instrList)
