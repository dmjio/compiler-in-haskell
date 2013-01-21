module Compile(compileFuncTo)
    where

import System.IO
import Parser.DLCParser

import Data.Map

-- current stack usage:
-- [(String, Type)]

type RegisterTable = [[String]]
registerTable :: RegisterTable
registerTable = [
    ["%rax", "%eax",  "%ax",  "%al"],
    ["%rdi", "%edi",  "%di", "%dil"],
    ["%rsi", "%esi",  "%si", "%sil"],
    ["%rdx", "%edx",  "%dx",  "%dl"],
    ["%rcx", "%ecx",  "%cx",  "%cl"],
    [ "%r8", "%r8d", "%r8w", "%r8b"],
    [ "%r9", "%r9d", "%r9w", "%r9b"]]

-- function signature map
type FuncSignMap = Map String (Type, [Type])
-- stack usage record
type StackUsageRec = [(String, Int)]

-- FIXME: number of params is restricted to no more than 6 for now
compileFuncTo :: Handle -> Func -> FuncSignMap -> IO ()
compileFuncTo f (retType, funcNameOrig, paramList, funcBody) fsMap =
    let
        fname = convDLCFuncName funcNameOrig
        (suRec, hea, foo) = funcASHeaderFooter paramList
        header = "pushq %rbp":"movq %rsp, %rbp":hea
        footer = foo ++ ["popq %rbp", "ret"]
    in do
        hPutStr f (".globl " ++ fname ++ "\n")
        hPutStr f (fname ++ ":\n")
        mapM_ (\s -> hPutStr f ("    " ++ s ++ "\n")) header
        mapM_ (\s -> hPutStr f ("    " ++ s ++ "\n")) footer
        hPutStr f "\n"
        putStrLn $ show fsMap -- DEBUG
        putStrLn $ show suRec -- DEBUG

convDLCFuncName :: String -> String
convDLCFuncName f = if f == "main" then "_dlc_main" else "__dlc_f_" ++ f

funcASHeaderFooter :: [(Type, String)] -> (StackUsageRec, [String], [String])
funcASHeaderFooter paramList =
    let
        (suRec, header, footer) = heafoo paramList $ tail registerTable
        -- helper method
        heafoo :: [(Type, String)] -> RegisterTable ->
                  (StackUsageRec, [String], [String])
        heafoo [] _ = ([], [], [])
        heafoo ((t,v):ps) (r:rs) =
            let
                size = typeToSize t         -- int i: 4
                rec = (v, size)             --        ("i", 4)
                -- reg = sizeToRegister size r          -- "%edi"
                reg = sizeToRegister 8 r    --        "%rdi"
                -- asSuffix = typeToSuffix t            -- "l"
                asSuffix = "q"              --        just use q
                pushCmd = "push" ++ asSuffix ++ " " ++ reg -- "pushl %edi"
                popCmd = "pop"   ++ asSuffix ++ " " ++ reg -- "popl  %edi"
                (suRec, he, fo) = heafoo ps rs
            in
                (rec:suRec, pushCmd:he, fo++[popCmd])
    in
        -- so ugly...
        if (length suRec) >= 2
        then (suRec, header, footer)
        else
            -- FIXME: should be Long_t and pushq when long is added.
            let suRec' = ("", 4):suRec
                header' = "push %rdi":header
                footer' = footer ++ ["pop %rdi"]
            in
                if (length suRec == 1)
                then (suRec', header', footer')
                else
                    let
                        suRec'' = ("", 4):suRec'
                        header'' = "push %rsi":header'
                        footer'' = footer' ++ ["pop %rsi"]
                    in
                        (suRec'', header'', footer'')

typeToSize :: Type -> Int
typeToSize Int_t = 4
typeToSize Void_t = 0

sizeToRegister :: Int -> [String] -> String
sizeToRegister 8 (x:_) = x
sizeToRegister 4 (_:x:_) = x
sizeToRegister 2 (_:_:x:_) = x
sizeToRegister 1 (_:_:_:x:_) = x

typeToSuffix :: Type -> String
typeToSuffix Int_t = "l"

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
