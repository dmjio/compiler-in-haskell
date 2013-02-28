module DLC.PrettyPrinter
    (prettyPrintTransResult, prettyPrintClassDef, prettyPrintMethodDef)
where

import DLC.TAST
import Data.Char (chr)
import Data.Map

prettyPrintTransResult :: Int -> TransResult -> IO ()
prettyPrintTransResult il (cdMap, mdMap) = do
    mapM_ (\cName -> prettyPrintClassDef il (cdMap ! cName)) (keys cdMap)
    mapM_ (\mName -> prettyPrintMethodDef il (mdMap ! mName)) (keys mdMap)

prettyPrintClassDef :: Int -> TClassDef -> IO ()
prettyPrintClassDef il (cName, superClassName, caList, cmList) = do
    indent il
    putStr $ "class " ++ cName
    if cName == "Object" then return () else putStr (" extends " ++ superClassName)
    putStrLn " {"
    mapM_ (ppClassAttrDef (il+1)) caList
    mapM_ (ppClassMethodDef (il+1)) cmList
    indent il
    putStrLn "}"

prettyPrintMethodDef :: Int -> TMethodDef -> IO ()
prettyPrintMethodDef il mDef = do {indent il; ppMethodDef il mDef}

ppClassAttrDef :: Int -> TClassAttrDef -> IO ()
ppClassAttrDef il (acc, isStatic, varDef) = do
    indent il
    ppClassAccessModifier acc
    putStr $ if isStatic then " static " else " "
    ppVarDef varDef
    putStrLn ";"

ppClassMethodDef :: Int -> TClassMethodDef -> IO ()
ppClassMethodDef il (acc, isStatic, mDef) = do
    indent il
    ppClassAccessModifier acc
    putStr $ if isStatic then " static " else " "
    ppMethodDef il mDef

-- take care! it doesn't indent on the first line...
ppMethodDef :: Int -> TMethodDef -> IO ()
ppMethodDef il (fName, fType, fArgList, fBody) = do
    ppType fType
    putStr $ " " ++ fName ++ "("
    case fArgList of
        [] -> return ()
        a:as -> do {ppArg a; mapM_ (\a -> do {putStr ", "; ppArg a}) as}
    putStrLn ") {"
    ppBodyList (il+1) fBody
    indent il
    putStrLn "}"
    where
        ppArg :: (String, TType) -> IO ()
        ppArg (aName, aType) = do {ppType aType; putStr $ " " ++ aName}

-- no prefix indent, no suffix ";" and newline
ppVarDef :: TVarDef -> IO ()
ppVarDef (vName, vType, vExpr) = do
    ppType vType
    putStr $ " " ++ vName ++ " = "
    ppExpr vExpr

ppClassAccessModifier :: TClassAccessModifier -> IO ()
ppClassAccessModifier acc =
    putStr $ case acc of
                TPublic -> "public"
                TProtected -> "protected"
                TPrivate -> "private"

ppType :: TType -> IO ()
ppType TVoid  = putStr "void"
ppType TInt   = putStr "int"
ppType TInt32 = putStr "int32"
ppType TByte  = putStr "byte"
ppType TBool  = putStr "bool"
ppType (TClass s) = putStr s
ppType (TArray aDepth tp) = do {ppType tp; mapM_ putStr $ take aDepth $ repeat "[]"}
-- report error for TUnknown?

ppBody :: Int -> TBodyStmt -> IO ()
ppBody il b =
    case b of
        (TBSStmt st) -> ppStmt il st
        (TBSExpr ep) -> do {indent il; ppExpr ep; putStrLn ";"}
    
ppBodyList :: Int -> [TBodyStmt] -> IO ()
-- ppBodyList _ [] = putStrLn ""
ppBodyList il bList = mapM_ (ppBody il) bList

ppStmt :: Int -> TStmt -> IO ()
ppStmt il st = do
    indent il
    case st of
        (TStmtVarDef vDef) -> do {ppVarDef vDef; putStrLn ";"}
        (TStmtPrint e) -> do {putStr "print("; ppExpr e; putStrLn ");"}
        (TStmtIf c b1 b2) -> do
            putStr "if ("
            ppExpr c
            putStrLn ") {"
            ppBodyList (il+1) b1
            indent il
            putStrLn "} else {"
            ppBodyList (il+1) b2
            indent il
            putStrLn "}"
        (TStmtFor init cond c b) -> do
            putStr "for ("
            case init of
                (Left e) -> ppExpr e
                (Right (tp, vList)) -> do
                    ppType tp
                    pvlist vList
                    where
                        pvlist :: [(String, TExpr)] -> IO ()
                        pvlist [(v,e)] = do {(putStr $ " " ++ v ++ " = "); ppExpr e}
                        pvlist (a:as) = do {pvlist [a];
                                            mapM_ (\a -> do {putStr ","; pvlist [a]}) as}
            putStr "; "
            ppExpr cond
            putStr "; "
            ppExpr c
            putStrLn ") {"
            ppBodyList (il+1) b
            indent il
            putStrLn "}"
        (TStmtWhile e b) -> do
            putStr "while ("
            ppExpr e
            putStrLn ") {"
            ppBodyList (il+1) b
            indent il
            putStrLn "}"
        (TStmtDoWhile b e) -> do
            putStr "do {"
            ppBodyList (il+1) b
            indent il
            putStr "} while ("
            ppExpr e
            putStrLn ");"
        (TStmtReturn e) -> do {putStr "return "; ppExpr e; putStrLn ";"}

ppExpr :: TExpr -> IO ()
ppExpr (TExprFunCall maybeExpr fName exprList) = do
    case maybeExpr of
        (Just e) -> do {ppExpr e; putStr "."}
        Nothing  -> return ()
    putStr $ fName ++ "("
    pArgList exprList
    putStr ")"
    where
        pArgList :: [TExpr] -> IO ()
        pArgList [] = return ()
        pArgList (a:as) = do {ppExpr a; pArgList' as}
        pArgList' :: [TExpr] -> IO ()
        pArgList' [] = return ()
        pArgList' (a:as) = do {putStr ", "; ppExpr a; pArgList' as}
ppExpr (TExprAdd e1 e2)       = do {ppExpr e1; putStr " + "  ; ppExpr e2}
ppExpr (TExprMin e1 e2)       = do {ppExpr e1; putStr " - "  ; ppExpr e2}
ppExpr (TExprMul e1 e2)       = do {ppExpr e1; putStr " * "  ; ppExpr e2}
ppExpr (TExprDiv e1 e2)       = do {ppExpr e1; putStr " / "  ; ppExpr e2}
ppExpr (TExprNeg e)           = do {           putStr "-"    ; ppExpr e }
ppExpr (TExprAnd e1 e2)       = do {ppExpr e1; putStr " and "; ppExpr e2}
ppExpr (TExprOr e1 e2)        = do {ppExpr e1; putStr " or " ; ppExpr e2}
ppExpr (TExprNot e)           = do {           putStr "not " ; ppExpr e }
ppExpr (TExprIncV e)          = do {           putStr "++"   ; ppExpr e }
ppExpr (TExprDecV e)          = do {           putStr "--"   ; ppExpr e }
ppExpr (TExprVInc e)          = do {ppExpr e;  putStr "++"              }
ppExpr (TExprVDec e)          = do {ppExpr e;  putStr "--"              }
ppExpr (TExprIncBy e1 e2)     = do {ppExpr e1; putStr " += " ; ppExpr e2}
ppExpr (TExprDecBy e1 e2)     = do {ppExpr e1; putStr " -= " ; ppExpr e2}
ppExpr (TExprEq e1 e2)        = do {ppExpr e1; putStr " == " ; ppExpr e2}
ppExpr (TExprNeq e1 e2)       = do {ppExpr e1; putStr " != " ; ppExpr e2}
ppExpr (TExprLeq e1 e2)       = do {ppExpr e1; putStr " <= " ; ppExpr e2}
ppExpr (TExprGeq e1 e2)       = do {ppExpr e1; putStr " >= " ; ppExpr e2}
ppExpr (TExprLe e1 e2)        = do {ppExpr e1; putStr " < "  ; ppExpr e2}
ppExpr (TExprGe e1 e2)        = do {ppExpr e1; putStr " > "  ; ppExpr e2}
ppExpr (TExprArrAccess e1 e2) = do {ppExpr e1; putStr "["    ; ppExpr e2; putStr "]"}
ppExpr (TExprDotAccess e1 s)  = do {ppExpr e1; putStr $ "." ++ s}
ppExpr (TExprBool b)          = putStr (if b then "true" else "false")
ppExpr (TExprVar v)           = putStr v
ppExpr (TExprInt v)           = putStr $ show v
ppExpr (TExprStr s)           = putStr $ show s -- for parens and escaping
ppExpr (TExprChar c)          = putStr $ show $ chr c
ppExpr TExprNull              = putStr "null"
ppExpr (TExprConvType t e)    = do {putStr "("; ppType t; putStr ")"; ppExpr e}
ppExpr (TExprAssign e1 e2)    = do {ppExpr e1; putStr " = "; ppExpr e2}
ppExpr (TExprNewObj s)        = putStr $ "new " ++ s ++ "()"
ppExpr (TExprNewArr t eList)  = do {putStr "new "; ppType t;
                                    mapM_ (\e -> do {putStr "["; ppExpr e; putStr "]"}) eList}

indent :: Int -> IO ()
indent n | n >= 1  = do {putStr "    "; indent (n-1)}
         | n == 0  = return ()
