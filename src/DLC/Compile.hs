module DLC.Compile
    (compileTo)
where

import Data.Map hiding (map, foldl)
import Data.Set hiding (map, foldl)
import Data.List (find)
import System.IO (hPutStr, hPutStrLn, Handle)
import Text.Regex.Posix

import DLC.CompileDataObject
import DLC.TAST

showSU :: [(String, TType)] -> String
-- showSU x = " # " ++ (show x)
showSU _ = ""

regTable :: [[String]]
regTable = [
    ["%rax", "%eax",  "%ax",  "%al"],
    ["%rdi", "%edi",  "%di", "%dil"],
    ["%rsi", "%esi",  "%si", "%sil"],
    ["%rdx", "%edx",  "%dx",  "%dl"],
    ["%rcx", "%ecx",  "%cx",  "%cl"],
    [ "%r8", "%r8d", "%r8w", "%r8b"],
    [ "%r9", "%r9d", "%r9w", "%r9b"]]

convFName :: String -> String
convFName f =
    if (f =~ "^__dlib.*$") :: Bool
    then drop 1 f
    else "___dl_" ++ f

compileTo :: Handle -> CDO -> IO ()
compileTo h cdo =
    let r1 = foldl (compileClass cdo) ([], []) (cdoGetClasses cdo)
        (asData, asText) = foldl (compileMethod cdo Nothing) r1 (cdoGetMethods cdo)
    in do
        writeDataSection h asData
        writeTextSection h asText

writeDataSection :: Handle -> [String] -> IO ()
writeDataSection h d = do
    hPutStrLn h ".data"
    mapM_ (\x -> if (x =~ "^.*:$") :: Bool
                 then do {hPutStrLn h ""; hPutStrLn h x}
                 else hPutStrLn h $ "    " ++ x)
          d

writeTextSection :: Handle -> [String] -> IO ()
writeTextSection h d = do
    hPutStrLn h ""
    hPutStrLn h ".text"
    mapM_ (\x -> if (x =~ "^\\.globl.*$") :: Bool
                 then do {hPutStrLn h ""; hPutStrLn h x}
                 else if (x =~ "^.*:$") :: Bool
                      then hPutStrLn h x
                      else hPutStrLn h ("    " ++ x))
          d

compileClass :: CDO -> ([String], [String]) -> String -> ([String], [String])
compileClass cdo (asData, asText) cName =
    let (_, superName, attrList, methodList) = cdoGetClassDef cdo cName
        -- generating method table; changes:
        --     asData => asData'
        methodTable :: [String]
        methodTable = cdoGetClassMethodTable cdo cName
        methodTable' :: [String] -- className$$methodName
        methodTable' = map (\cm -> case resolveClassName cm of
                                        Nothing -> error $ "cannot find def of " ++ cm
                                        Just x -> (x ++ "$" ++ cm))
                           methodTable
        sup = if cName == "Object" then "$0" else superName ++ "$$"
        asData' = asData ++ [cName ++ "$$:", ".quad " ++ sup] ++
                  map (\x -> ".quad " ++ (convFName x)) methodTable'
        resolveClassName :: String -> Maybe String
        resolveClassName cmName = r cName
            where r :: String -> Maybe String
                  r cName =
                    case cdoGetClassDef cdo cName of
                        (_, _, _, cmDefList) ->
                            if any (\(_, _, (m, _, _, _)) -> m == cmName) cmDefList
                            then Just cName
                            else if cName == "Object"
                                 then Nothing
                                 else r $ cdoGetSuperClass cdo cName
    in
        foldl (\r (_, _, (m, _, _, _)) -> compileMethod cdo (Just cName) r m)
              (asData', asText) methodList

-- when compileMethod is applied to class attribute methods,
-- the 2nd method is "Just superClassName".
-- else it should be Nothing.
compileMethod :: CDO -> Maybe String -> ([String], [String]) -> String -> ([String], [String])
compileMethod cdo cName (asData, asText) mName =
    let mName' = case cName of
                    (Just x) -> x ++ "$" ++ mName
                    Nothing -> mName
        cName' = case cName of
                    (Just x) -> x
                    Nothing -> ""
        mDef :: TMethodDef -- (String, TType, [(String, TType)], [TBodyStmt])
        (isStatic, mDef) = if cName /= Nothing
                           then case cdoGetClassMethodDef cdo cName' mName of
                                    (Just (_, y, x)) -> (y, x)
                                    Nothing -> error $ "no def of " ++ mName ++ " in " ++ cName'
                           else (False, cdoGetMethodDef cdo mName)
        (tp, argList, mBody) =
            case mDef of
                (_, mt, mArgs, mBody) ->
                    (mt, if cName /= Nothing && not isStatic
                         then ("this", TClass cName'):mArgs
                         else mArgs,
                     mBody)
        -- now we have:
        -- cName:   Maybe String       => class name
        -- mName:   String             => method name
        -- tp:      TType              => return type
        -- argList: [(String, TType)]  => argument list (modified for class attr methods)
        -- mBody:   [TBodyStmt]        => method body
        --
        -- mName':  String  => className$methodName or just methodName
        stackUsage :: [(String, TType)]
        stackUsage = argList ++
                     (if (length argList) == 0
                      then [("", TUnknown), ("", TUnknown)]
                      else if (length argList) == 1
                           then [("", TUnknown)]
                           else [])-- ++ argList
        beforeBody :: [String]
        beforeBody = ["push %rbp", "mov %rsp, %rbp"] ++
                     map (\(rIdx, _) -> "push " ++ (regTable !! rIdx !! 0))
                         (zip [1..6] stackUsage) ++
                     (concat $ map (\(offset, _) ->
                                        ["mov " ++ (show offset) ++ "(%rbp), %rax",
                                         "push %rax"])
                                   -- last arg: 16
                                   -- first arg: ???
                                   -- stack: arg7, arg8, (ret)
                                   $ reverse (zip [16, 24..] $ reverse $ drop 6 stackUsage))
        asDataInc :: [String]
        asMethodBody :: [String]
        su :: [(String, TType)]
        (_, asDataInc, asMethodBody, _, su, _) =
            foldl cBodyStmt
                  (cdo, [], [], 1, stackUsage, mName')
                  mBody
        afterBody :: [String]
        afterBody = ["add $" ++ (show $ 8 * (length su)) ++ ", %rsp",
                     "pop %rbp",
                     "ret"]
    in
        (asData ++ asDataInc,
         asText ++ [".globl " ++ (convFName mName'), (convFName mName') ++ ":"]
         ++ beforeBody ++ ["# begin mBody: " {-++ (show mBody)-}]
         ++ asMethodBody ++ ["# end mBody"] ++ afterBody)

isIntType :: TType -> Bool
isIntType TInt = True
isIntType TInt32 = True
isIntType TByte = True
isIntType _ = False

getIntTypeRank :: TType -> Int
getIntTypeRank TInt = 8
getIntTypeRank TInt32 = 4
getIntTypeRank TByte = 1

-- for assignment / function call
canCoerceInto :: CDO -> TType -> TType -> Bool
-- canCoerceInto _ TVoid TVoid = True -- FIXME: MAYBE this should not be True.
canCoerceInto _ TVoid _     = False
canCoerceInto _ _     TVoid = False
canCoerceInto _ TUnknown _ = True
canCoerceInto cdo (TClass cFrom) (TClass cTo) =
    if cFrom == cTo
    then True
    else if cFrom == "Object"
         then False
         else canCoerceInto cdo (TClass $ cdoGetSuperClass cdo cFrom) (TClass cTo)
canCoerceInto cdo (TArray dep1 tp1) (TArray dep2 tp2) =
    if dep1 /= dep2
    then False
    else canCoerceInto cdo tp1 tp2
canCoerceInto cdo t1 t2 | t1 == t2  = True
                        | otherwise =
    let s = Data.Set.fromList [TInt, TInt32, TByte] -- bool canNOT be converted.
    in Data.Set.member t1 s && Data.Set.member t2 s

-- for +, -, *, /
coerce :: TType -> TType -> TType
coerce t1 t2 = if getIntTypeRank t1 > getIntTypeRank t2 then t1 else t2

-- checkCanCoerceInto :: CDO -> TType -> TType -> Bool
-- checkCanCoerceInto cdo t1 t2 =
--     if canCoerceInto cdo t1 t2
--     then True
--     else error $ "cannot coerce " ++ (show t1) ++ " into " ++ (show t2) -- TROLOLOLOL


-- 0: the first arg, "-8(%rbp)".
getStackVar :: Int -> String
getStackVar n = "-" ++ (show $ (n + 1) * 8) ++ "(%rbp)"
    
--                 .data     .text  jmpTag  stack usage      method name
type CArg = (CDO, [String], [String], Int, [(String, TType)], String)

cBodyStmt :: CArg -> TBodyStmt -> CArg
cBodyStmt x (TBSStmt s) = cStmt (caAppendText ["# stmt: " ++ (show s)] x) s
cBodyStmt x (TBSExpr e) =
    let (cdo, d, t, jt, su, fn) = cExpr (caAppendText [ "# expr: " ++ (show e)] x) e
    in (cdo, d,
        t ++ ["pop %rdi"], -- pop the temp var out of stack
        jt,
        init su, -- clean su record
        fn)

getJTag :: String -> Int -> String
getJTag f n = "JTag_" ++ f ++ "_" ++ (show n)

caAppendText :: [String] -> CArg -> CArg
caAppendText t (a, b, c, d, e, f) = (a, b, c++t, d, e, f)

caAddJTag :: Int -> CArg -> CArg
caAddJTag n (a, b, c, d, e, f) = (a, b, c, d+n, e, f)

caGetJTagN :: CArg -> Int
caGetJTagN (_, _, _, a, _, _) = a

getFuncReturnType :: CDO -> String -> Maybe TType
getFuncReturnType cdo fName = -- fName: "C$func" or "func"
    let mName = reverse $ takeWhile ('$' /=) $ reverse fName
        cName = let t = take (length fName - length mName) fName
                in if length t == 0
                   then t -- "func" => ""
                   else init t -- "C$func" => "C"
    in if cName == ""
       then case (cdoGetMethodDef cdo mName) of
                (_, tp, _, _) -> Just tp
       else case (cdoGetClassMethodDef cdo cName mName) of
                Nothing -> Nothing
                Just (_, _, (_, tp, _, _)) -> Just tp

cStmt :: CArg -> TStmt -> CArg
cStmt ca@(cdo, d, t, j, su, mn) (TStmtVarDef (vName, tp, expr)) = -- int i = 1;
    case Prelude.lookup vName su of
        -- cExpr will push result of the expression onto the stack,
        -- so no need to pop the temporary variable out in assembly.
        Nothing -> let (_, d', t', j', su', _) = cExpr ca expr
                       (_, tp_e) = last su'
                   in if canCoerceInto cdo tp_e tp
                      then (cdo, d', t', --  ++ ["push " ++ (getStackVar $ length su')],
                            j', su ++ [(vName, tp)], mn) -- yes, su, but not su'
                      else error $ "vardef: cannot coerce " ++ (show tp_e) ++
                                   " into " ++ (show tp)
        -- variables defined inside blocks(for, while...) will be the same as if they are
        -- defined at beginning of the whole method.
        Just x -> error $ "multiple definition of " ++ vName ++ " in " ++ mn

-- print("hi") => "hi".__dl_putstr()
cStmt ca@(cdo, dataSec, textSec, jt, su, mn) (TStmtPrint e) = -- print(15)
    let (_, dataSec', textSec', _, su', _) = cExpr ca e
        (_, tp_e) = last su'
        ptname = if tp_e == TInt || tp_e == TInt32
                 then "__dlib_print_num"
                 else case tp_e of
                        TByte -> "__dlib_print_char"
                        TBool -> "__dlib_print_bool"
        padding = ((length su) `mod` 2) * 8
        callcText = ["pop " ++ (regTable !! 1 !! 0),
                     "sub $" ++ (show padding) ++ ", %rsp",
                     "call " ++ (convFName ptname), -- no return value, only one arg
                     "add $" ++ (show padding) ++ ", %rsp"]
    in if tp_e == TArray 1 TByte
       then case cExpr ca (TExprFunCall (Just e) "__dl_putstr" []) of
                (cdo, dS, tS, jt, su, mn) -> (cdo, dS, tS ++ ["add $8, %rsp"], jt, init su, mn)
       else (cdo, dataSec', textSec' ++ callcText, jt, su, mn) -- not su'

cStmt ca@(cdo, dataSec, textSec, _, su, fName) (TStmtIf e b1 b2) = -- if (c) {b1} else {b2}
    let (_, dataSec', textSec', jt, su', _) =
            cExpr (caAppendText ["# before if, su: " ++ (show su)] ca) e
        (_, tp_e) = last su'
        elseT = getJTag fName jt
        endT = getJTag fName (jt+1)
        condSec = ["pop %rdi", "cmpb $0, %dil", "jz " ++ elseT]
        ca' = (cdo, dataSec', textSec' ++ condSec, jt + 2, su, fName)
        ca'' = caAppendText ["jmp " ++ endT, elseT ++ ":"]
                            (foldl cBodyStmt ca' b1)

        ca'''@(_, _, _, _, su''', _) = caAppendText [endT ++ ":"] (foldl cBodyStmt ca'' b2)
    in if not (tp_e == TBool || tp_e == TUnknown)
       then error $ "expression in if's cond section must be boolean, found " ++ (show tp_e)
       else caAppendText ["# after if, su: " ++ (show su''')] ca'''

-- for (initS; condS; incrS) {b}
--   =>
-- initS;
-- while(condS) {b; incrS;}
-- [undef]
cStmt ca@(cdo, dataSec, textSec, jt, su, fName) (TStmtFor initS condS incrS b) =
    let whileStmt = TStmtWhile condS (b ++ [TBSExpr incrS])
        ca' = case initS of
                Left e -> cExpr (caAppendText ["# before for"] ca) e
                Right (tp, varList) ->
                    foldl (\a (vName, e) -> cStmt a (TStmtVarDef (vName, tp, e)))
                          (caAppendText ["# before for"] ca) varList
        nTVar = case initS of
                    Left _ -> 0
                    Right (_, vl) -> length vl
        (_, dataSec', textSec', jt', su', _) = cStmt ca' whileStmt
        su'' = reverse $ drop nTVar $ reverse su'
        undefS = if nTVar /= 0
                 then ["add $" ++ (show (nTVar * 8)) ++ ", %rsp # undef"]
                 else []
    in (cdo, dataSec', textSec' ++ undefS , jt', su'', fName)

-- sTag:
--  if not e:
--      goto eTag
--  [b]
--  goto sTag
-- eTag:
cStmt ca@(cdo, dataSec, textSec, jt, su, fName) (TStmtWhile e b) = -- while(e) {b;}
    let sTag = getJTag fName jt
        eTag = getJTag fName (jt + 1)
        (_, dataSec', textSec', jt', su', _) =
            cExpr (cdo, dataSec,
                   textSec ++ ["# begin while", sTag ++ ":"],
                   jt + 2, su, fName) e
        (_, tp_e) = last su'
        checkCondSec = ["pop %rdi",
                        "cmpb $0, %dil",
                        "jz " ++ eTag]
        ca' = (cdo, dataSec', textSec' ++ checkCondSec, jt', su, fName) -- su, not su'
        ca'' = caAppendText ["jmp " ++ sTag, eTag ++ ":"] (foldl cBodyStmt ca' b)
    in if not (tp_e == TBool || tp_e == TUnknown)
       then error "expression in while's cond section must be bool"
       else ca''

-- FIXME: do-while


-- FIXME: "return;" with no argument for void methods
cStmt ca@(cdo, _, _, _, su, fName) (TStmtReturn e) =
    let ca'@(cdo', dS', tS', jt', su', mn') = cExpr ca e
        (_, tp_e) = last su'
        tp = case getFuncReturnType cdo fName of
                    Just x -> x
        cleanUp = ["pop %rax",
                   "add $" ++ (show $ (length su) * 8) ++ ", %rsp",
                   "pop %rbp",
                   "ret"]
    in if {- tp == TVoid || -} canCoerceInto cdo tp_e tp
       then -- stack usage doesn't contain the returned value.
            -- (as if return is a function call)
            -- or it would raise issues for return inside a if branch.
            caAppendText cleanUp (cdo', dS', tS', jt', init su', mn')
       else error $ "return value type for " ++ fName ++ " is incorrect"


-- -- for non-static class methods only.
-- -- static class methods don't use "this".
-- getCurrentClassName :: CArg -> Maybe String
-- getCurrentClassName (_, _, _, _, su, _) =
--     case filter (\(v, t) -> v == "this") of
--         [] -> Nothing
--         ((_, (TClass s)):_) -> Just s
hasThisInStack :: CArg -> Bool
hasThisInStack (_, _, _, _, su, _) = any (\(v, _) -> v == "this") su

-- for regular/C-bridge methods only
getMethodSignature :: CArg -> String -> Maybe (TType, [TType])
getMethodSignature ((_, _, mDefMap, cmDefMap), _, _, _, _, _) mName =
    case Data.Map.lookup mName mDefMap of
        Just (_, retTp, argList, _) -> Just (retTp, map (\(_, t) -> t) argList)
        Nothing -> case Data.Map.lookup mName cmDefMap of
                    Just r -> Just r
                    Nothing -> Nothing
-- for class methods only (no matter static or not);
-- includes "this" for non-static methods.
getClassMethodSignature :: CArg -> String -> String ->
                           Maybe (String, TClassAccessModifier, Bool, TType, [TType])
getClassMethodSignature ca@((_, cDefMap, _, _), _, _, _, _, _) cName mName =
    case Data.Map.lookup cName cDefMap of
        Nothing -> Nothing
        Just (_, supClass, _, mDefList) ->
            case find (\(_, _, (f, _, _, _)) -> f == mName) mDefList of
                Just (acc, isStatic, (_, retTp, argList, _)) ->
                    Just (cName, acc, isStatic, retTp,
                          (if isStatic then [] else [TClass cName]) ++ map (\(_, t) -> t) argList)
                Nothing -> if cName == "Object"
                           then Nothing
                           else getClassMethodSignature ca supClass mName

-- return true iff cs != cb and cs is superclass of cb.
isSuperClassOf :: CArg -> String -> String -> Bool
isSuperClassOf ca@(cdo, _, _, _, _, _) cs cb =
    if cs == cb
    then False
    else f' cdo cs cb
    where
        f' :: CDO -> String -> String -> Bool
        f' cdo cs cb =
            if cb == "Object"
            then False
            else let s = cdoGetSuperClass cdo cb
                 in if s == cs
                    then True
                    else f' cdo cs s

cExpr :: CArg -> TExpr -> CArg

-- (this.)hello()
-- (MyClass.)hello()
-- or just hello()
--
-- o.hello()
-- this.hello()
-- super.hello()
-- MyClass.hello()
-- 
-- non-static class methods:
--      fName => "MyClass$func", has "this" in su
-- static class methods:
--      fName => "MyClass$func", no "this" in su
-- regular methods:
--      fName => "func", no "this" in su
cExpr ca@(_, _, _, _, _, cf) (TExprFunCall maybeE f args) =
    case maybeE of
        Nothing ->
            if not insideClassBody
            then callRegF
            else case getClassMethodSignature ca curClass f of
                    Nothing -> callRegF
                    Just (c, acc, isStatic, retTp, tpList) ->
                        if isStatic && (c == curClass || acc == TPublic || acc == TProtected)
                        then callFunc ca (c ++ "$" ++ f) args tpList retTp
                        else error $ "cannot call " ++ c ++ "$" ++ f ++ " inside " ++ cf
        Just e ->
            let (eIsClassName, eClassName) = exprIsClassName e
            in if eIsClassName -- MyClass.func()
               then case getClassMethodSignature ca eClassName f of
                        Just (c, acc, isStatic, retTp, tpList) ->
                            if isStatic && (acc == TPublic || curClass == c ||
                                            (curClass /= "" &&
                                             acc == TProtected &&
                                             isSuperClassOf ca c curClass))
                            then callFunc ca (eClassName ++ "$" ++ f) args tpList retTp
                            else error $ "cannot call " ++ eClassName ++ "$" ++ f ++
                                         " inside " ++ cf
                        Nothing -> error $ "cannot call " ++ eClassName ++ "$" ++ f
               else callObjFunc ca e f args -- e.func() [might be super.func()]
    where
        exprIsClassName :: TExpr -> (Bool, String)
        exprIsClassName (TExprVar s) =
            let matches = ((s =~ "^_*[A-Z][A-Za-z0-9_]*$") :: Bool)
            in if matches
               then (True, s)
               else (False, "")
        exprIsClassName _ = (False, "")

        callRegF :: CArg
        callRegF = case getMethodSignature ca f of
                            Nothing -> error $ "cannot find function " ++ f
                            Just (retTp, argTpList) -> callFunc ca f args argTpList retTp

        insideClassBody = any ('$' ==) cf
        curFName = reverse $ takeWhile ('$' /=) $ reverse cf
        curClass = if insideClassBody
                   then take ((length cf) - 1 - (length curFName)) cf
                   else ""
        -- callObjFunc: prepareArguments, __calFuncPos then pop to rax__, popArguments,
        --              alignStack, __call rax__, unalignStack&push rax&setSU
        -- callFunc: prepareArguments, popArguments,
        --           alignStack, __call f__, unalignStack&push rax&setSU
        -- void func: push a dumb var onto stack?
        
        -- prepareArguments: calculate arguments, do args type checking, push them onto stack
        prepareArguments :: CArg -> [TExpr] -> [TType] -> CArg
        prepareArguments orig_ca@(_, _, _, _, su, _) args tpList =
            if length args /= length tpList
            then error $ "wrong number of args"
            else let args' = (drop 6 args) ++ (reverse $ take 6 args)
                     tpList' = (drop 6 tpList) ++ (reverse $ take 6 tpList)
                     -- orig: 1 2 3 4 5 6 7 8
                     -- now: 7 8 6 5 4 3 2 1
                     ca = -- caAppendText ["# before prepareArguments, old su: " ++ (show su)]
                                       orig_ca
                     ca'@(cdo', dataSec', textSec', jt', su', mn') = foldl cExpr ca args'
                     ca'' = caAppendText ["# new su: " ++ (show su')]
                     wrongTp = find (\((_, sutp), argTp) -> not $ canCoerceInto cdo' sutp argTp)
                                    (zip (reverse su') (reverse tpList'))
                 in case wrongTp of
                        Just ((vName, sutp), argTp) ->
                            error $ "prepareArguments: cannot coerce " ++ (show sutp) ++
                                    " <" ++ vName ++ "> into " ++ (show argTp) ++
                                    " when calling " ++ f
                        Nothing -> ca'
        -- popArguments: pop calculated arguments (which are on stack) into registers.
        --               stack usage will be also modified.
        popArguments :: CArg -> Int -> CArg
        popArguments ca@(cdo, dataSec, textSec, jt, su, mn) argN =
            let (popArgs, _) =
                    foldl (\(acc, rt) _ -> (acc ++ ["pop " ++ ((head rt) !! 0)], drop 1 rt))
                          ([], drop 1 regTable)
                          (take argN $ repeat 0xDEADBEEF)
                su' = reverse $ drop argN $ reverse su
            in (cdo, dataSec, textSec ++ popArgs {- ++
                              ["# old su: " ++ (show su),
                               "# after popArguments, new su: " ++ (show su')] -},
                jt, su', mn)
        alignStack :: CArg -> (Int, CArg)
        alignStack ca@(cdo, dataSec, textSec, jt, su, mn) =
            if paddingSize == 0
            then (paddingSize, ca)
            else (paddingSize,
                  (cdo, dataSec,
                   textSec ++ ["sub $" ++ (show paddingSize) ++ ", %rsp # " ++ (show su)],
                   jt, su, mn)) -- take care: su doesn't contain the padding variable
            where
                paddingSize = ((length su) * 8) `mod` 16
        unalignStack :: CArg -> Int -> CArg
        unalignStack ca@(cdo, dataSec, textSec, jt, su, mn) paddingSize =
            if paddingSize == 0
            then ca
            else (cdo, dataSec, textSec ++ ["add $" ++ (show paddingSize) ++ ", %rsp"],
                  jt, su, mn)
        callObjFunc :: CArg -> TExpr -> String -> [TExpr] -> CArg
        callObjFunc ca objExpr f args =
            if objExpr == (TExprVar "super") && ((not insideClassBody) || curClass == "Object")
            then error "cannot access super"
            else let ca'@(cdo, d, t, jt, su, mn) = -- su: includes objExpr
                            cExpr ca (case objExpr of
                                        TExprVar "super" -> TExprVar "this"
                                        _ -> objExpr)
                     -- maybeRetTp is used for "generic" methods;
                     -- aka. __DL_Array.__dl_get().
                     maybeRetTp :: Maybe TType
                     (objClass, maybeRetTp) =
                        case last su of
                            (_, TClass objClass) -> (objClass, Nothing)
                            -- RefArray doesn't add any new method;
                            -- and also RefArray is not exposed to the user,
                            -- so we could just use __DL_Array here.
                            (_, TArray n tp) -> if f == "__dl_get"
                                                then ("__DL_Array",
                                                      if n == 1
                                                      then Just tp
                                                      else Just (TArray (n-1) tp))
                                                else ("__DL_Array",
                                                      -- RefArray doesn't add new method
                                                      Nothing)
                     su' = (init su) ++ [("$dlc_obj", TClass objClass)] -- su: has objExpr
                     Just (cName, acc, False, retTp, tpList) =
                        getClassMethodSignature ca' objClass f
                     -- su in ca'': ..., objExpr, [orignial arguments], objExpr again
                     ca'' = prepareArguments (cdo, d, t, jt, su', mn)
                                             ((TExprVar "$dlc_obj"):args)
                                             tpList
                     fOffset = cdoGetFuncOffset cdo objClass f
                     cTableS = ["movq (%rsp), %rax", -- get pointer to the obj
                                "movq (%rax), %rax"] ++ -- get pointer to the MT
                               (if objExpr == TExprVar "super"
                                then ["movq (%rax), %rax"]
                                else []) ++
                               ["add $" ++ (show fOffset) ++ ", %rax",
                                "mov (%rax), %rax"] -- get addr of the function
                     (padding, ca''') = -- ca''': has objExpr, padding missing
                        alignStack $ popArguments (caAppendText cTableS ca'')
                                                  (length tpList)

                     -- +8: for the extra $dlc_obj
                     -- ca'''': ..., objExpr
                     ca'''' = unalignStack (caAppendText ["callq *%rax # " ++ f] ca''')
                                           (padding + 8)
                 in -- FIXME: allow accessing protected/private non-static attribute methods
                    --        from anywhere for now...
                    -- if objClass /= cName && acc == TPrivate
                    -- then error $ "cannot call private method " ++ f
                    -- else
                    case ca'''' of
                        (cdo', d', t', jt', su', mn') ->
                            let su = (init su') ++
                                     [("", case maybeRetTp of
                                                Nothing -> retTp
                                                Just tp -> tp)]
                            in (cdo', d', t' ++ ["push %rax" ++ (showSU su)],
                               jt', su, mn')


        callFunc :: CArg -> String -> [TExpr] -> [TType] -> TType -> CArg
        callFunc ca f args argTpList retTp =
            let ca' = prepareArguments ca args argTpList
                (padding, ca'') = alignStack $ popArguments ca' $ length args
                ca'''@(cdo, d, t, jt, su, mn) =
                    unalignStack (caAppendText ["call " ++ (convFName f)] ca'') padding
            in (cdo, d, t ++ ["push %rax # hey!"], jt, su ++ [("", retTp)], mn)

cExpr ca (TExprAdd e1 e2) =
    let (cdo, dataSec, textSec, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, e1tp), (_, e2tp)] = drop (length su - 2) su
        tp = coerce e1tp e2tp --     e2           e1        e1 += e2
        textSec' = textSec ++ ["pop %rsi", "pop %rdi", "add %rsi, %rdi", "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", tp)]
    in (cdo, dataSec, textSec', jt, su', mn)
cExpr ca (TExprMin e1 e2) =
    let (cdo, dataSec, textSec, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, e1tp), (_, e2tp)] = drop (length su - 2) su
        tp = coerce e1tp e2tp --     e2           e1        e1 -= e2
        textSec' = textSec ++ ["pop %rsi", "pop %rdi", "sub %rsi, %rdi", "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", tp)]
    in (cdo, dataSec, textSec', jt, su', mn)
-- FIXME: use imul/idiv ?
cExpr ca (TExprMul e1 e2) =
    let (cdo, dataSec, textSec, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, e1tp), (_, e2tp)] = drop (length su - 2) su
        tp = coerce e1tp e2tp --     e2           e1        e1 *= e2
        textSec' = textSec ++ ["pop %rsi", "pop %rdi", "imulq %rsi, %rdi", "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", tp)]
    in (cdo, dataSec, textSec', jt, su', mn)
cExpr ca (TExprDiv e1 e2) =
    let (cdo, dataSec, textSec, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, e1tp), (_, e2tp)] = drop (length su - 2) su
        tp = coerce e1tp e2tp --     e2           e1        e1 /= e2
        textSec' = textSec ++ ["pop %rsi", "pop %rdi", "idivq %rsi, %rdi", "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", tp)]
    in (cdo, dataSec, textSec', jt, su', mn)

cExpr ca (TExprNeg e) =
    let (cdo, dataSec, textSec, jt, su, mn) = cExpr ca e
        (_, tp) = last su --          e          rdi = 0        rdi -= e
        textSec' = textSec ++ ["pop %rsi", "mov $0, %rdi", "sub %rsi, %rdi", "push %rdi"]
        su' = (init su) ++ [("", tp)]
    in if isIntType tp
       then (cdo, dataSec, textSec', jt, su', mn)
       else error "cannot apply neg to non-int types"
cExpr ca (TExprAnd e1 e2) =
    let (cdo, dataSec, textSec, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, e1tp), (_, e2tp)] = drop (length su - 2) su
        textSec' = textSec ++ ["pop %rsi", "pop %rdi", "and %rsi, %rdi", "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", TBool)]
    in if e1tp == TBool && e2tp == TBool
       then (cdo, dataSec, textSec', jt, su', mn)
       else error "cannot apply and to non-bool types"
cExpr ca (TExprOr e1 e2) =
    let (cdo, dataSec, textSec, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, e1tp), (_, e2tp)] = drop (length su - 2) su
        textSec' = textSec ++ ["pop %rsi", "pop %rdi", "or %rsi, %rdi", "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", TBool)]
    in if e1tp == TBool && e2tp == TBool
       then (cdo, dataSec, textSec', jt, su', mn)
       else error "cannot apply or to non-bool types"
cExpr ca (TExprNot e) =
    let (cdo, dataSec, textSec, jt, su, mn) = cExpr ca e
        (_, tp) = last su
        textSec' = textSec ++ ["pop %rdi", "xor $1, %rdi", "push %rdi"]
        su' = (init su) ++ [("", TBool)]
    in if tp /= TBool
       then error "cannot apply not to non-bool types"
       else (cdo, dataSec, textSec', jt, su', mn)

-- ++, --, +=, -=

cExpr ca (TExprIncV e) = cExpr ca (TExprIncBy e $ TExprInt 1) -- ++i
cExpr ca (TExprDecV e) = cExpr ca (TExprDecBy e $ TExprInt 1) -- --i
cExpr ca (TExprVInc e) = -- i++
    let (cdo, dataSec, textSec, jt, su, mn) = cExpr ca (TExprIncBy e $ TExprInt 1)
    in (cdo, dataSec, textSec ++ ["pop %rdi", "sub $1, %rdi", "push %rdi"], jt, su, mn)
cExpr ca (TExprVDec e) = -- i--
    let (cdo, dataSec, textSec, jt, su, mn) = cExpr ca (TExprDecBy e $ TExprInt 1)
    in (cdo, dataSec, textSec ++ ["pop %rdi", "add $1, %rdi", "push %rdi"], jt, su, mn)
        
-- a[s] += e   =>   a.__dl_set(s, e+a[s])
cExpr ca (TExprIncBy (TExprArrAccess eArr eSub) e) =
    let (cdo, dataSec, textSec, jt, su, mn) = foldl cExpr ca [eArr, eSub, e]
        [(_, t1), (_, t2), (_, t3)] = drop (length su - 3) su
        su' = (take (length su - 3) su) ++
              [("$dlc_eArr", t1), ("$dlc_eSub", t2), ("$dlc_e", t3)]
        ca''@(cdo'', d'', t'', jt'', su'', mn'') =
                cExpr (cdo, dataSec, textSec, jt, su', mn)
                      (TExprFunCall (Just $ TExprVar "$dlc_eArr")
                                    "__dl_set"
                                    [TExprVar "$dlc_eSub",
                                     (TExprAdd (TExprArrAccess (TExprVar "$dlc_eArr")
                                                               (TExprVar "$dlc_eSub"))
                                               (TExprVar "$dlc_e"))])
        tS = ["mov " ++ getStackVar (length su + 2) ++ ", %rax",
              "add $32, %rsp",
              "push %rax # hey!"]
        su''' = su ++ [("", coerce t2 t3)]
    in (cdo'', d'', t'' ++ tS, jt'', su''', mn'')

cExpr ca (TExprDecBy (TExprArrAccess eArr eSub) e) =
    let (cdo, dataSec, textSec, jt, su, mn) = foldl cExpr ca [eArr, eSub, e]
        [(_, t1), (_, t2), (_, t3)] = drop (length su - 3) su
        su' = (take (length su - 3) su) ++
              [("$dlc_eArr", t1), ("$dlc_eSub", t2), ("$dlc_e", t3)]
        ca''@(cdo'', d'', t'', jt'', su'', mn'') =
                cExpr (cdo, dataSec, textSec, jt, su', mn)
                      (TExprFunCall (Just $ TExprVar "$dlc_eArr")
                                    "__dl_set"
                                    [TExprVar "$dlc_eSub",
                                     (TExprMin (TExprArrAccess (TExprVar "$dlc_eArr")
                                                               (TExprVar "$dlc_eSub"))
                                               (TExprVar "$dlc_e"))])
        tS = ["mov " ++ getStackVar (length su + 2) ++ ", %rax",
              "add $32, %rsp",
              "push %rax"]
        su''' = su ++ [("", coerce t2 t3)]
    in (cdo'', d'', t'' ++ tS, jt'', su''', mn'')

-- cExpr ca (TExprIncBy e1 e2) =
--     let (cdo, dataSec, textSec, jt, su, mn) = foldl cExpr ca [e1, e2]
--         [(_, e1tp), (_, e2tp)] = take (length su - 2) su
--         tp = coerce e1tp e2tp
--         textSec' = textSec ++ ["pop"]

-- a += n or a.b += n
--
-- rax = ptr
-- rdi = ptr
-- rdi = *rdi
-- rsi = n
-- rdi += rsi
-- *rax = rdi
-- push rdi
cExpr ca (TExprIncBy e1 e2) =
    let (cdo, dS, tS, jt, su, mn) = cExpr (cExprAddr ca e1) e2 -- foldl cExpr ca [e1, e2]
        [(_, t1), (_, t2)] = drop (length su - 2) su
        tS' = tS ++ ["pop %rsi", "pop %rax", "mov %rax, %rdi", "mov (%rdi), %rdi",
                     "add %rsi, %rdi", "mov %rdi, (%rax)", "push %rdi"]
        tp = coerce t1 t2
        su' = (take (length su - 2) su) ++ [("", tp)]
    in (cdo, dS, tS', jt, su', mn)
cExpr ca (TExprDecBy e1 e2) =
    let (cdo, dS, tS, jt, su, mn) = cExpr (cExprAddr ca e1) e2 -- foldl cExpr ca [e1, e2]
        [(_, t1), (_, t2)] = drop (length su - 2) su
        tS' = tS ++ ["pop %rsi", "pop %rax", "mov %rax, %rdi", "mov (%rdi), %rdi",
                     "sub %rsi, %rdi", "mov %rdi, (%rax)", "push %rdi"]
        tp = coerce t1 t2
        su' = (take (length su - 2) su) ++ [("", tp)]
    in (cdo, dS, tS', jt, su', mn)

-- bool a == bool b  =>  a ^ b ^ 1
-- else:             =>  not (TBool)(a-b)
cExpr ca (TExprEq e1 e2) =
    let (cdo, dS, tS, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, t1), (_, t2)] = drop (length su - 2) su
        tS' = tS ++ ["pop %rdi", "pop %rsi", "xor %rsi, %rdi", "xor $1, %rdi", "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", TBool)]
    in if t1 == TBool && t2 == TBool
       then (cdo, dS, tS', jt, su', mn)
       else cExpr ca (TExprNot $ TExprConvType TBool $ TExprMin e1 e2)

-- bool a != bool b  =>  a ^ b
-- else:             =>  (TBool)(a-b)
cExpr ca (TExprNeq e1 e2) =
    let (cdo, dS, tS, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, t1), (_, t2)] = drop (length su - 2) su
        tS' = tS ++ ["pop %rdi", "pop %rsi", "xor %rsi, %rdi", "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", TBool)]
    in if t1 == TBool && t2 == TBool
       then (cdo, dS, tS', jt, su', mn)
       else cExpr ca $ TExprConvType TBool $ TExprMin e1 e2

cExpr ca (TExprLeq e1 e2) =
    let (cdo, dS, tS, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, t1), (_, t2)] = drop (length su - 2) su
        tag1 = getJTag mn jt
        tag2 = getJTag mn (jt+1)
        jt' = jt + 2
        tS' = tS ++ ["pop %rsi", "pop %rdi",
                     "cmpq %rsi, %rdi", -- cmp e2, e1
                     "jle " ++ tag1,   -- jle T1
                     "mov $0, %rdi",   --     mov $0, %rdi
                     "jmp " ++ tag2,   --     jmp T2
                     tag1 ++ ":",      -- T1:
                     "mov $1, %rdi",   --     mov $1, %rdi
                     tag2 ++ ":",      -- T2:
                     "push %rdi"]      -- push %rdi
        su' = (take (length su - 2) su) ++ [("", TBool)]
    in if (isIntType t1) && (isIntType t2)
       then (cdo, dS, tS', jt', su', mn)
       else error "cannot compare non-int types"
cExpr ca (TExprGeq e1 e2) =
    let (cdo, dS, tS, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, t1), (_, t2)] = drop (length su - 2) su
        tag1 = getJTag mn jt
        tag2 = getJTag mn (jt+1)
        jt' = jt + 2
        tS' = tS ++ ["pop %rsi", "pop %rdi",
                     "cmpq %rsi, %rdi",
                     "jge " ++ tag1,
                     "mov $0, %rdi",
                     "jmp " ++ tag2,
                     tag1 ++ ":",
                     "mov $1, %rdi",
                     tag2 ++ ":",
                     "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", TBool)]
    in if (isIntType t1) && (isIntType t2)
       then (cdo, dS, tS', jt', su', mn)
       else error "cannot compare non-int types"
cExpr ca (TExprLe e1 e2) =
    let (cdo, dS, tS, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, t1), (_, t2)] = drop (length su - 2) su
        tag1 = getJTag mn jt
        tag2 = getJTag mn (jt+1)
        jt' = jt + 2
        tS' = tS ++ ["pop %rsi", "pop %rdi",
                     "cmpq %rsi, %rdi",
                     "jl " ++ tag1,
                     "mov $0, %rdi",
                     "jmp " ++ tag2,
                     tag1 ++ ":",
                     "mov $1, %rdi",
                     tag2 ++ ":",
                     "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", TBool)]
    in if (isIntType t1) && (isIntType t2)
       then (cdo, dS, tS', jt', su', mn)
       else error "cannot compare non-int types"
cExpr ca (TExprGe e1 e2) =
    let (cdo, dS, tS, jt, su, mn) = foldl cExpr ca [e1, e2]
        [(_, t1), (_, t2)] = drop (length su - 2) su
        tag1 = getJTag mn jt
        tag2 = getJTag mn (jt+1)
        jt' = jt + 2
        tS' = tS ++ ["pop %rsi", "pop %rdi",
                     "cmpq %rsi, %rdi",
                     "jg " ++ tag1,
                     "mov $0, %rdi",
                     "jmp " ++ tag2,
                     tag1 ++ ":",
                     "mov $1, %rdi",
                     tag2 ++ ":",
                     "push %rdi"]
        su' = (take (length su - 2) su) ++ [("", TBool)]
    in if (isIntType t1) && (isIntType t2)
       then (cdo, dS, tS', jt', su', mn)
       else error "cannot compare non-int types"

-- int[][] a;
-- a[s] => (int[]) a.__dl_get(s)
cExpr ca (TExprArrAccess eArr eSub) =
    let (_, _, _, _, su, _) = cExpr ca eArr
        (_, TArray dep tp) = last su
        tp' = if dep == 1
              then tp
              else TArray (dep-1) tp
    in cExpr ca $ TExprConvType tp' $ TExprFunCall (Just eArr) "__dl_get" [eSub]

cExpr ca (TExprDotAccess e v) =
    let (cdo@(oi, cDefMap, _, _), dS, tS, jt, su, mn) = cExpr ca e
        (_, TClass cName) = last su
        -- FIXME: acc and isStatic ignored
        Just (_, _, (_, tp, _)) = cdoGetClassAttrDef cdo cName v
        -- (_, _, tp) = case cDefMap ! cName of
        --                 (_, _, attrList, _) ->
        --                     case find (\(_, _, (vn, _, _)) -> vn == v) attrList of
        --                         Just (acc, isStatic, (_, tp, _)) -> (acc, isStatic, tp)
        --                         Nothing -> error $ "cExpr dotAccess: cannot find " ++ v
        getOffset :: CDO -> String -> String -> Maybe Int
        getOffset cdo@(oi, _, _, _) cName v =
            case Data.Map.lookup cName oi of
                Just (_, _, m) -> case Data.Map.lookup v m of
                                    Just x -> Just x
                                    Nothing -> if cName == "Object"
                                                then Nothing
                                                else getOffset cdo (cdoGetSuperClass cdo cName) v
        offset = case getOffset cdo cName v of Just x -> x
        tS' = tS ++ ["pop %rdi", "mov " ++ (show offset) ++ "(%rdi), %rdi", "push %rdi"]
        su' = (init su) ++ [("", tp)]
    in (cdo, dS, tS', jt, su', mn)

cExpr (cdo, dS, tS, jt, su, mn) (TExprBool b) =
    (cdo, dS, tS ++ ["push $" ++ if b then "1" else "0"], jt, su ++ [("", TBool)], mn)
cExpr (cdo, dS, tS, jt, su, mn) (TExprVar v) =
    let (idx, (_, tp)) = case find (\(i, (vn, tp)) -> vn == v) $ zip [0..] su of
                            Just x -> x
                            Nothing -> error $ "cannot find variable " ++ v
        tS' = tS ++ ["mov " ++ getStackVar idx ++ ", %rdi", "push %rdi"]
    in (cdo, dS, tS', jt, su ++ [("", tp)], mn)
cExpr (cdo, dS, tS, jt, su, mn) (TExprInt n) =
    (cdo, dS, tS ++ ["push $" ++ (show n)], jt, su ++ [("", TInt)], mn)

cExpr (cdo, dS, tS, jt, su, mn) (TExprStr s) =
    let strTag = "STAG_" ++ mn ++ "_" ++ (show jt)
        jt' = jt + 1
        padding = (length su `mod` 2) * 8
        dS' = dS ++ [strTag ++ ":", ".ascii " ++ (show s)]
        tS' = tS ++ ["leaq " ++ strTag ++ "(%rip), " ++ (regTable !! 1 !! 0),
                     "mov $" ++ (show $ length s) ++ ", " ++ (regTable !! 2 !! 0),
                     "mov $4, " ++ (regTable !! 3 !! 0),
                     "sub $" ++ (show padding) ++ ", %rsp",
                     "call " ++ (convFName "__DL_Array$__dl_copyFromCBytes"),
                     "add $" ++ (show padding) ++ ", %rsp",
                     "push %rax"]
        su' = su ++ [("", TArray 1 TByte)]
    in (cdo, dS', tS', jt', su', mn)

cExpr (cdo, dS, tS, jt, su, mn) (TExprChar c) =
    (cdo, dS, tS ++ ["push $" ++ (show c)], jt, su ++ [("", TByte)], mn)
cExpr (cdo, dS, tS, jt, su, mn) TExprNull =
    (cdo, dS, tS ++ ["push $0"], jt, su ++ [("", TUnknown)], mn)

cExpr ca (TExprConvType tp e) =
    let ca'@(cdo, dS, tS, jt, su, mn) = cExpr ca e
        (_, e_tp) = last su
        _tBoolConvert = tp == TBool && e_tp /= TBool
        jt' = if _tBoolConvert then jt + 1 else jt
        tag = getJTag mn jt
        su' = (init su) ++ [("", tp)]
        tS' = if _tBoolConvert
              then tS ++ ["pop %rax",       -- pop %rax
                          "cmpq $0, %rax",   -- cmp %rax, $0
                          "jz " ++ tag,    -- jz T
                          tag ++ ":",      -- T:
                          "mov $1, %rax",   --    mov $1, %rax
                          "push %rax"]      -- push %rax
              else tS
    in (cdo, dS, tS', jt', su', mn)

-- speed up for vars
-- notice: it is not correct on the types.
--         read the general implementation when uncomment this.
--
-- cExpr ca (TExprAssign (TExprVar v) e) =
--     let ca'@(cdo, dS, tS, jt, su, mn) = cExpr ca e
--         Just (idx, (_, vTp)) = find (\(idx, (vn, _)) -> vn == v) $ zip [0..] su
--         (_, eTp) = last su
--         tp = if (eTp == TUnknown) || (vTp == TBool && eTp == TBool)
--              then vTp -- might set bool to non-0-1 value?
--              else if isIntType eTp && isIntType vTp
--                   then coerce eTp vTp
--                   else if canCoerceInto cdo eTp vTp
--                        then vTp
--                        else error $ "cannot coerce " ++ (show eTp) ++ " into " ++ (show vTp)
--         su' = (init su) ++ [("", tp)]
--         tS' = tS ++ ["mov " ++ (getStackVar (length su - 1)) ++ ", %rax", -- note: not pop
--                      "mov %rax, " ++ (getStackVar idx)]
--     in (cdo, dS, tS', jt, su', mn)

-- a[s] = e  ->  a.__dl_set(s, e) && return e
cExpr ca (TExprAssign (TExprArrAccess eArr eSub) e) =
    let ca'@(cdo, dS, tS, jt, su, mn) = foldl cExpr ca [eArr, eSub, e]
        [(_, aTp), (_, sTp), (_, eTp)] = drop (length su - 3) su
        apTp = case aTp of
                TArray 1 t -> t
                TArray n t -> TArray (n-1) t
        -- su': ..., arr, sub, e
        su' = (take (length su - 3) su) ++
              [("$dlc_arr", aTp), ("$dlc_sub", sTp), ("$dlc_e", eTp)]
        -- su'': ..., arr, sub, e, TVoid
        ca''@(cdo'', dS'', tS'', jt'', su'', mn'') =
            cExpr (cdo, dS, tS, jt, su', mn)
                  (TExprFunCall (Just $ TExprVar "$dlc_arr")
                                "__dl_set"
                                [TExprVar "$dlc_sub",
                                 (TExprConvType TInt $ TExprVar "$dlc_e")])
        su''' = (take (length su'' - 4) su'') ++ [("", apTp)]
        tS''' = tS'' ++ ["add $8, %rsp", "pop %rax", "add $16, %rsp", "push %rax"]
    in if canCoerceInto cdo eTp apTp
       then (cdo'', dS'', tS''', jt'', su''', mn'')
       else error $ "cExpr arr access: cannot coerce " ++ (show eTp) ++ " into " ++ (show apTp)

cExpr ca (TExprAssign e1 e2) =
    let ca'@(cdo, dS, tS, jt, su, mn) = cExpr (cExprAddr ca e1) e2
        [(_, vTp), (_, eTp)] = drop (length su - 2) su
        tp = if (eTp == TUnknown) || (vTp == TBool && eTp == TBool) ||
                (isIntType eTp && isIntType vTp) || (canCoerceInto cdo eTp vTp)
             then vTp
             else error $ "cExpr assign: cannot coerce " ++ (show eTp) ++ " into " ++ (show vTp)
        su' = (take (length su - 2) su) ++ [("", tp)]
        tS' = tS ++ ["pop %rdi", "pop %rsi", "mov %rdi, (%rsi)", "push %rdi"]
    in (cdo, dS, tS', jt, su', mn)

-- FIXME: call init
cExpr (cdo@(oi, _, _, _), dS, tS, jt, su, mn) (TExprNewObj cName) =
    let (oSize, _, _) = case Data.Map.lookup cName oi of Just x -> x
        padding = ((length su) `mod` 2) * 8
        s = ["mov $" ++ (show oSize) ++ ", %rdi",
             "sub $" ++ (show padding) ++ ", %rsp",
             "call _malloc",
             "add $" ++ (show padding) ++ ", %rsp",
             -- "movq " ++ cName ++ "$$, (%rax)",
             "leaq " ++ cName ++ "$$(%rip), %rdi",
             "movq %rdi, (%rax)",
             "push %rax"]
    in (cdo, dS, tS ++ s, jt, su ++ [("", TClass cName)], mn)

-- new int[35]  =>  RefArray of Array
-- new C[1]     =>  RefArray of RefArray
cExpr ca (TExprNewArr tp [e]) =
    let ca'@(cdo, dS, tS, jt, su, mn) = cExpr ca e
        bsize = if tp == TBool || tp == TByte
                then 1
                else case tp of
                        TInt -> 8
                        TInt32 -> 4
                        TClass _ -> 8
        arrType = case tp of
                        TClass _ -> "__DL_RefArray"
                        _ -> "__DL_Array"
        -- su contains len, which will be popped out
        -- so we need to -1.
        padding = ((length su - 1) `mod` 2) * 8
        s = ["pop %rdi",                            -- len
             "sub $" ++ (show padding) ++ ", %rsp",
             "mov $" ++ (show $ bsize) ++ ", %rsi", -- bsize
             "call " ++ (convFName "__DL_Array$__dl_create"),
             "add $" ++ (show padding) ++ ", %rsp"] ++
            (case tp of
                 TClass _ -> ["leaq __DL_RefArray$$(%rip), %rdi",
                              "movq %rdi, (%rax)"]
                 _ -> []) ++
            ["push %rax"]
        su' = (init su) ++ [("", TArray 1 tp)]
    in (cdo, dS, tS ++ s, jt, su', mn)

-- cExpr ca (TExprNewArr tp (e:es)) = -- FIXME: implement it!

-- if the addr points to a int,
-- type on su should also be int (but NOT int*).
cExprAddr :: CArg -> TExpr -> CArg
cExprAddr ca@(cdo, dS, tS, jt, su, mn) (TExprVar v) =
    let Just (idx, (_, tp)) = find (\(_, (vn, _)) -> vn == v) $ zip [0..] su
        tS' = tS ++ ["mov %rbp, %rdi",
                     "sub $" ++ (show $ (idx+1)*8) ++ ", %rdi",
                     "push %rdi"]
    in (cdo, dS, tS', jt, su ++ [("", tp)], mn)

cExprAddr ca (TExprDotAccess e v) =
    let (cdo@(oi, cDefMap, _, _), dS, tS, jt, su, mn) = cExpr ca e
        (_, TClass cName) = last su
        -- Just (_, _, (_, vTp, _)) = -- FIXME: acc and isStatic are ignored
        --     case cDefMap ! cName of
        --         (_, _, attrList, _) ->
        --             find (\(acc, isStatic, (vn, tp, _)) -> vn == v) attrList
        Just (_, _, (_, vTp, _)) = cdoGetClassAttrDef cdo cName v
        vOffset = case Data.Map.lookup cName oi of
                    Just (_, _, m) -> case Data.Map.lookup v m of Just x -> x
        tS' = tS ++ ["pop %rax", "add $" ++ (show vOffset) ++ ", %rax", "push %rax"]
        su' = init su ++ [("", vTp)]
    in (cdo, dS, tS', jt, su', mn)
