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
-- the integer is how much space the element uses;
-- but because of the limitation of push/pop, each element always
-- use 8 bytes.
type StackUsageRec = [(String, Int)]

-- FIXME: number of params is restricted to no more than 6 for now
compileFuncTo :: Handle -> Func -> FuncSignMap -> IO ()
compileFuncTo f (retType, funcNameOrig, paramList, funcBody) fsMap =
    let
        fname = convDLCFuncName funcNameOrig
        (suRec, hea, foo) = funcASHeaderFooter paramList
        header = "pushq %rbp":"movq %rsp, %rbp":hea
        footer = foo ++ ["popq %rbp", "ret"]
        body = compFuncBody fsMap suRec funcBody
    in do
        hPutStr f (".globl " ++ fname ++ "\n")
        hPutStr f (fname ++ ":\n")
        mapM_ (\s -> hPutStr f ("    " ++ s ++ "\n")) header
        mapM_ (\s -> hPutStr f ("    " ++ s ++ "\n")) footer
        hPutStr f "\n"
        putStrLn $ show fsMap -- DEBUG
        putStrLn $ show suRec -- DEBUG

compFuncBody :: FuncSignMap -> StackUsageRec -> [Either Stmt Expr] ->
                [String]
compFuncBody _ suRec [] =
    -- FIXME: wait... why don't I just write "mov %rbp, %rsp"?
    ["add %rsp, $" ++ (show ((length suRec) * 8)) ++ "\n",
     "ret\n"]
compFuncBody fsMap suRec ((Left stmt):xs) =
    let (suRec', asCode) = compStmt fsMap suRec stmt
    in  asCode ++ (compFuncBody fsMap suRec' xs)
compFuncBody fsMap suRec ((Right expr):xs) =
    (compExpr fsMap suRec expr) ++
    ["add $8, %rsp\n"] ++ -- the return value is ignored
    (compFuncBody fsMap suRec xs)

-- statements may change the stack usage record.
-- (e.g., int i;)
compStmt :: FuncSignMap -> StackUsageRec -> Stmt -> (StackUsageRec, [String])
compStmt fsMap suRec (ReturnStmt expr) =
    let
        exprCode = compExpr fsMap suRec expr
        stackSizeStr = show $ (length suRec) * 8
    in
        exprCode ++ ["movq -" ++ stackSizeStr ++ "(%rbp), %rax\n"] ++
        -- take advantage of compFuncBody here...
        -- notice: we must also pop the temporary value out,
        -- so a dumb record is added.
        compFuncBody fsMap (suRec ++ [("", 4)]) []
compStmt _ suRec (DefStmt vType vName) =
    -- be careful: the order is important for accessing the variable.
    (suRec ++ [(vName, typeToSize vType)],
     ["sub $8, %rsp\n"])
compStmt fsMap suRec (AssignStmt vName expr) =
    let
        exprCode = compExpr fsMap suRec expr
        varOffset = (length (takeWhile (\(s, _) -> s != vName) suRec) + 1) * 8
    in
        exprCode ++
        ["pop %rdi\n",
         "movq %rdi, -" ++ (show varOffset) ++ "(%rbp)\n"]

-- finally push the result onto the bottom of stack.
-- (e.g., where %rsp currently points to)
--
-- ** NOTICE **
-- the tempary value push onto the stack is NOT stored in StackUsageRecord.
-- be careful!!!!!!!!
compExpr :: FuncSignMap -> StackUsageRec -> Expr -> [String]


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
