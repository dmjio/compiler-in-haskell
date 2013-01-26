module Compile(compileFuncTo)
    where

import System.IO
import Parser.DLCParser
import AST

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
        funcEndTag = "DLC_FUNC_END" ++ fname
        (suRec, hea, foo) = funcASHeaderFooter paramList
        header = "pushq %rbp":"movq %rsp, %rbp":hea
        footer = [funcEndTag ++ ":"] ++ foo ++ ["popq %rbp", "ret"]
        body = compFuncBody fsMap suRec funcBody funcEndTag
    in do
        hPutStr f (".globl " ++ fname ++ "\n")
        hPutStr f (fname ++ ":\n")
        mapM_ (\s -> hPutStr f ("    " ++ s ++ "\n")) header
        mapM_ (\s -> hPutStr f ("    " ++ s ++ "\n")) body
        mapM_ (\s -> hPutStr f ("    " ++ s ++ "\n")) footer
        hPutStr f "\n"
        -- putStrLn $ show fsMap -- DEBUG
        -- putStrLn $ show suRec -- DEBUG

compFuncBody :: FuncSignMap -> StackUsageRec -> [Either Stmt Expr] -> String ->
                [String]
compFuncBody _ suRec [] endTag =
    -- FIXME: wait... why don't I just write "mov %rbp, %rsp"?
    ["add $" ++ (show ((length (dropWhile (\(vName, _) -> vName == "") suRec)) * 8) ++ ", %rsp"),
     "jmp " ++ endTag]
compFuncBody fsMap suRec ((Left stmt):xs) endTag =
    let (suRec', asCode) = compStmt fsMap suRec stmt endTag
    in  asCode ++ (compFuncBody fsMap suRec' xs endTag)
compFuncBody fsMap suRec ((Right expr):xs) endTag =
    (compExpr fsMap suRec expr) ++
    ["add $8, %rsp"] ++ -- the return value is ignored
    (compFuncBody fsMap suRec xs endTag)

-- statements may change the stack usage record.
-- (e.g., int i;)

-- FIXME: there should be an return statement for void...
compStmt :: FuncSignMap -> StackUsageRec -> Stmt -> String -> (StackUsageRec, [String])
compStmt fsMap suRec (ReturnStmt expr) endTag =
    let
        exprCode = compExpr fsMap suRec expr
        stackSizeStr = show $ ((length suRec) + 1) * 8
        -- tricky:
        --     compFuncBody will pop out all locally-defined variables
        --     (and also parameters, which are treated as if they are defined
        --      inside functions);
        --     it will recognize variables at the beginning of suRec which have
        --     "" as its name as the spots we make for the registers when number
        --     of parameters is less than 2.
        --     so here I'm giving it a dumb variable name to prevent the variable
        --     being treated as the spot made for the registers.
        --
        --     notice that this is an invalid DL variable name, so it will conflict
        --     with variables defined within functions.
        guard_var_name = "**DL_DUMB_VARNAME**"
    in
        (suRec, exprCode ++
                ["movq -" ++ stackSizeStr ++ "(%rbp), %rax"] ++ -- return value
                -- take advantage of compFuncBody here...
                -- notice: we must also pop the temporary value out,
                -- so a dumb record is added.
                compFuncBody fsMap (suRec ++ [(guard_var_name, 4)]) [] endTag)
compStmt _ suRec (DefStmt vType vName) _ =
    -- be careful: the order is important for accessing the variable.
    (suRec ++ [(vName, typeToSize vType)],
     ["sub $8, %rsp"])
compStmt fsMap suRec (AssignStmt vName expr) _ =
    let
        exprCode = compExpr fsMap suRec expr
        varOffset = (length (takeWhile (\(s, _) -> s /= vName) suRec) + 1) * 8
    in
        (suRec, exprCode ++
                ["pop %rdi",
                 "movq %rdi, -" ++ (show varOffset) ++ "(%rbp)"])

-- finally push the result onto the bottom of stack.
-- (e.g., where %rsp currently points to)
--
-- ** NOTICE **
-- the tempary value push onto the stack is NOT stored in StackUsageRecord.
-- be careful!!!!!!!!
compExpr :: FuncSignMap -> StackUsageRec -> Expr -> [String]
compExpr fsMap suRec (FunCallExpr fName exprList) =
    let
        exprCode = Prelude.foldl (++) [] $ Prelude.map (compExpr fsMap suRec) exprList
        popCode = Prelude.map (\r -> "pop %" ++ r) $
                      reverse $ take (length exprList)
                                     ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
        fName' = if member fName fsMap
                 then convDLCFuncName fName
                 else "_" ++ fName
    in
        -- if it's an expression, it is supposed to have a return value...
        exprCode ++ popCode ++ ["call " ++ fName', "push %rax"]
compExpr fsMap suRec (AddExpr e1 e2) = compE1_WHAT_E2 fsMap suRec e1 e2 "addq"
compExpr fsMap suRec (SubExpr e1 e2) = compE1_WHAT_E2 fsMap suRec e1 e2 "subq"
compExpr fsMap suRec (MulExpr e1 e2) = compE1_WHAT_E2 fsMap suRec e1 e2 "imulq"
compExpr fsMap suRec (DivExpr e1 e2) = compE1_WHAT_E2 fsMap suRec e2 e1 "divq" -- special!
compExpr fsMap suRec (NegExpr expr) =
    (compExpr fsMap suRec expr) ++ ["pop %rax", "negq %rax", "push %rax"]
compExpr _     _     (IntExpr v) = ["push $" ++ (show v)]
compExpr _     suRec (VarExpr vName) =
    let
        varPos = ((length $ takeWhile (\(v, _) -> v /= vName) suRec) + 1) * 8
    in
        ["mov -" ++ (show varPos) ++ "(%rbp), %rax", "push %rax"]

-- compute the expression "e2 ?? e1".
-- I cannot find a good name, so decided to just give it a weird one.
compE1_WHAT_E2 :: FuncSignMap -> StackUsageRec -> Expr -> Expr -> String -> [String]
compE1_WHAT_E2 fsMap suRec e1 e2 what =
    (concat $ Prelude.map (compExpr fsMap suRec) [e1, e2]) ++
    ["pop %rax", "pop %rdi", what ++ " %rdi, %rax", "push %rax"]

-- convert the function name written in DLC into assembly style.
-- notice: this is not supposed to be called on C bridge functions.
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
                (suRec, he, _) = heafoo ps rs
            in
                -- we will let the parameters be poped just like variables
                -- defined inside functions.
                (rec:suRec, pushCmd:he, [])
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


sizeToRegister :: Int -> [String] -> String
sizeToRegister 8 (x:_) = x
sizeToRegister 4 (_:x:_) = x
sizeToRegister 2 (_:_:x:_) = x
sizeToRegister 1 (_:_:_:x:_) = x

sizeToSuffix :: Int -> String
sizeToSuffix 8 = "q"
sizeToSuffix 4 = "l"
sizeToSuffix 2 = "w"
sizeToSuffix 1 = "b"

typeToSuffix :: Type -> String
typeToSuffix = sizeToSuffix . typeToSize
