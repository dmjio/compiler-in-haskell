module DLC.ASTTransformer
    (transformParserAST)
where

import Data.Map
import Control.Monad(foldM)

import Parser.ParserAST
import DLC.Job
import DLC.TAST

-- FIXME: where the hell would errLog jump out?

-- "(var).h":
--   if var matches '_*[A-Z][A-Za-z_0-9]*',
--     it means accessing the static member h of class (var);
--   if it matches '_*[a-z][A-Za-z0-9]*',
--     it means accessing non-static member h of object (var).
--   else (var) is a illegal name.
--
-- TBodyStmt is used by both method definitions and control statements
--   (if, for, while, do-while).
--
-- "do {int i = 1;} while (i != 0);" should produce "error: i not defined"
-- if i is not defined elsewhere.

transformParserAST :: [PRootElem] -> Job TransResult
transformParserAST res = Ok (empty, empty) >>= (tpAST res) where
    tpAST :: [PRootElem] -> TransResult -> Job TransResult
    tpAST [] tr        = return tr
    tpAST (re:res) tr  = transRootElem re tr >>= (tpAST res)

transRootElem :: PRootElem -> TransResult -> Job TransResult
transRootElem re (cdMap, mdMap) =
    case re of
        (PRootClassDef p) ->
            let r = transClassDef p
            in case r of
                (Ok cDef@(cName, _, _, _)) -> return $ (insert cName cDef cdMap, mdMap)
                (ErrorLog eLog)            -> ErrorLog eLog -- yes this is necessary for ghc
        (PRootMethodDef p) ->
            let r = transMethodDef p
            in case r of
                (Ok mDef@(mName, _, _, _)) -> return $ (cdMap, insert mName mDef mdMap)
                (ErrorLog eLog)            -> ErrorLog eLog

transClassDef :: PClassDef -> Job TClassDef
transClassDef (className, superClassName, classBody) =
    let
        -- Object is a super class of it self? LOL
        -- take care: when searching methods, remember to stop if cannot find it
        --            in Object. Or the program will loop forever.
        superClassName' = case superClassName of
                            Nothing  -> "Object"
                            (Just x) -> x
        t = foldM transClassBody ([], []) classBody
    in
        case t of
            Ok (mDefList, aDefList) -> Ok (className, superClassName', aDefList, mDefList)
            (ErrorLog eLog)         -> ErrorLog eLog

transClassBody :: ([TClassMethodDef], [TClassAttrDef]) ->
                  Either PClassMethodDef PClassAttrDef ->
                  Job ([TClassMethodDef], [TClassAttrDef])
transClassBody (cmDefList, caDefList) (Left pcmDef) = -- class method
    let
        (maybeAccMod, isStatic, pmDef) = pcmDef
        accMod = transAccessMod maybeAccMod
        mDefJob = transMethodDef pmDef
    in
        case mDefJob of
            (Ok tmDef)      -> return (cmDefList ++ [(accMod, isStatic, tmDef)], caDefList)
            (ErrorLog eLog) -> ErrorLog eLog
transClassBody (cmDefList, caDefList) (Right pcaDef) = -- class attribute
    let
        (maybeAccMod, isStatic, (tp, varPairList)) = pcaDef
        accMod = transAccessMod maybeAccMod
        tp' = transType tp
        aDefList = Prelude.foldl (\x (n, e) ->
                                    x ++ [(accMod, isStatic, (n, tp', transMaybeExpr tp' e))])
                                 [] varPairList
    in
        Ok (cmDefList, caDefList ++ aDefList)

-- TType is used for providing a default value if the second param is Nothing.
transMaybeExpr :: TType -> Maybe PExpr -> TExpr
transMaybeExpr tp Nothing = case tp of
                                (TArray _ _) -> TExprNull
                                _            -> TExprInt 0
transMaybeExpr _ (Just expr) = transExpr expr

transType :: PType -> TType
transType (PArray t) =
    case transType t of
        (TArray n tp) -> TArray (n+1) tp
        tp            -> TArray 1 tp
transType (PObjClass s) = TClass s
transType t | t == PVoid  = TVoid
            | t == PInt   = TInt
            | t == PInt32 = TInt32
            | t == PByte  = TByte
            | t == PBool  = TBool

transMethodDef :: PMethodDef -> Job TMethodDef
transMethodDef (pType, mName, args, mBody) =
    let tp' = transType pType
        args' = Prelude.map (\(tp, n) -> (n, transType tp)) args
        mBody' = transSentenceList mBody
    in  Ok (mName, tp', args', mBody')

transSentenceList :: [PSentence] -> [TBodyStmt]
transSentenceList ps = concat $ Prelude.map conv ps
    where conv :: PSentence -> [TBodyStmt]
          conv (Left stmt) = Prelude.map TBSStmt $ transStmt stmt
          conv (Right expr) = [TBSExpr $ transExpr expr]

transAccessMod :: Maybe PClassAccessModifier -> TClassAccessModifier
transAccessMod Nothing = TPublic
transAccessMod (Just x) | x == PPublic    = TPublic
                        | x == PProtected = TProtected
                        | x == PPrivate   = TPrivate

transStmt :: PStmt -> [TStmt]
transStmt (PStmtVarDef (tp, varList)) =
    let tp' = transType tp
    in Prelude.map (\(n, maybePExpr) -> TStmtVarDef (n, tp', transMaybeExpr tp' maybePExpr))
                   varList
transStmt (PStmtPrint []) = [TStmtPrint $ TExprChar 10] -- 10: '\n'; print() => print newline
transStmt (PStmtPrint es) = Prelude.map (TStmtPrint . transExpr) es
transStmt (PStmtIf c b1 b2) =
    [TStmtIf (transExpr c) (transSentenceList b1) (transSentenceList b2)]
transStmt (PStmtFor (Left (PStmtVarDef (tp, vList))) e2 e3 b) =
    let tp' = transType tp
        vList' = Prelude.map (\(n, maybePExpr) -> (n, tp', transMaybeExpr tp' maybePExpr)) vList
        b' = transSentenceList b
    in  [TStmtFor (Right vList') (transExpr e2) (transExpr e3) b']
transStmt (PStmtFor (Right e1) e2 e3 b) =
    [TStmtFor (Left $ transExpr e1) (transExpr e2) (transExpr e3) (transSentenceList b)]
transStmt (PStmtWhile e b) = [TStmtWhile (transExpr e) (transSentenceList b)]
transStmt (PStmtDoWhile b e) = [TStmtDoWhile (transSentenceList b) (transExpr e)]
transStmt (PStmtReturn e) = [TStmtReturn (transExpr e)]

transExpr :: PExpr -> TExpr
transExpr (PExprFunCall Nothing fName exprList) =
    TExprFunCall Nothing              fName $ Prelude.map transExpr exprList
transExpr (PExprFunCall (Just e) fName exprList) =
    TExprFunCall (Just $ transExpr e) fName $ Prelude.map transExpr exprList
transExpr (PExprAdd e1 e2) = TExprAdd (transExpr e1) (transExpr e2)
transExpr (PExprMin e1 e2) = TExprMin (transExpr e1) (transExpr e2)
transExpr (PExprMul e1 e2) = TExprMul (transExpr e1) (transExpr e2)
transExpr (PExprDiv e1 e2) = TExprDiv (transExpr e1) (transExpr e2)
transExpr (PExprNeg e)     = TExprNeg (transExpr e)
transExpr (PExprAnd e1 e2) = TExprAnd (transExpr e1) (transExpr e2)
transExpr (PExprOr  e1 e2) = TExprOr  (transExpr e1) (transExpr e2)
transExpr (PExprNot e)     = TExprNot (transExpr e)
transExpr (PExprIncV e)    = TExprIncV (transExpr e)
transExpr (PExprDecV e)    = TExprDecV (transExpr e)
transExpr (PExprVInc e)    = TExprVInc (transExpr e)
transExpr (PExprVDec e)    = TExprVDec (transExpr e)
transExpr (PExprIncBy e1 e2) = TExprIncBy (transExpr e1) (transExpr e2)
transExpr (PExprDecBy e1 e2) = TExprDecBy (transExpr e1) (transExpr e2)
transExpr (PExprEq    e1 e2) = TExprEq    (transExpr e1) (transExpr e2)
transExpr (PExprNeq   e1 e2) = TExprNeq   (transExpr e1) (transExpr e2)
transExpr (PExprLeq   e1 e2) = TExprLeq   (transExpr e1) (transExpr e2)
transExpr (PExprGeq   e1 e2) = TExprGeq   (transExpr e1) (transExpr e2)
transExpr (PExprLe    e1 e2) = TExprLe    (transExpr e1) (transExpr e2)
transExpr (PExprGe    e1 e2) = TExprGe    (transExpr e1) (transExpr e2)
transExpr (PExprArrAccess e1 e2) = TExprArrAccess (transExpr e1) (transExpr e2)
transExpr (PExprDotAccess e s) = TExprDotAccess (transExpr e) s
transExpr (PExprBool b)      = TExprBool b
transExpr (PExprVar s) = TExprVar s
transExpr (PExprInt v) = TExprInt v
transExpr (PExprStr s) = TExprStr s
transExpr (PExprChar c) = TExprChar c
transExpr PExprNull = TExprNull
transExpr (PExprConvType tp e) = TExprConvType (transType tp) (transExpr e)
transExpr (PExprAssign e1 e2) = TExprAssign (transExpr e1) (transExpr e2)
transExpr (PExprNewObj s) = TExprNewObj s
transExpr (PExprNewArr tp eList) = TExprNewArr (transType tp) (Prelude.map transExpr eList)

-- transClassBody :: [Either PClassMethodDef PClassAttrDef] ->
--                   ([TClassMethodDef], [TClassAttrDef]) ->
--                   Job ([TClassMethodDef], [TClassAttrDef])
-- transClassBody (cb:cbs) (caDefList, cmDefList) =
--     ...
--     where
--         case cb of
--             (Left (maybeMod, isStatic, mDef))
        

-- -- type IdMapVal = Either TClassDef TMethodDef
-- -- type IdMap = (Map String IdMapVal)
-- -- type TransResult = Either IdMap String
-- type TransResult = (Map String TClassDef, Map String TMethodDef)
-- type TransResult = ([String, TClassDef], [String, TMethodDef])
-- 
-- type TClassDef =
-- type TMethodDef =
-- type TClassMethodDef =
-- type TClassAttrVarDef =
-- 
-- addTransResult :: Either (Map a b) String -> Either (a, b) String ->
--                   Either (Map a b) String
-- addTransResult t@(Right errLog) _ = t
-- addTransResult _ t@(Right errLog) = t
-- addTransResult (Left m) (k, v) = Left $ insert k v m
-- 
-- addTransResults :: Either (Map a b) String -> [Either (a, b) String] ->
--                    Either (Map a b) String
-- addTransResults m kvs = foldl addTransResult m kvs
-- 
-- 
-- transformParserAST :: [PRootElem] -> TransResult
-- transformParserAST pr = tpAST pr (Left empty) where
--     tpAST :: [PRootElem] -> TransResult -> TransResult
--     tpAST (e:es) t = tpAST (addTransResult t (transParserRootElem e)) es
-- 
-- transParserRootElem :: PRootElem -> Either [(String, IdMapVal)] String
-- transParserRootElem (PRootClassDef (className, superClassName, classBody)) =
--     let
--         (cMethodDefs, cAttrDefs) =
--             foldl (\(mds, ads) b -> case b of
--                                         (Left md) -> (mds ++ [md], ads)
--                                         (Right ad) -> (mds, ads ++ [ad]))
--                   ([], []) classBody
--         mDefResult :: Either (Map String TClassMethodDef) String
--         mDefResult =  addTransResults empty $ transParserClassMethodDefs cMethodDefs
--         aDefResult :: Either (Map String TClassAttrDef) String
--         aDefResult =  addTransResults empty $ transParserClassAttrDefs cAttrDefs
-- 
--         mDefResult' :: Either [(String, IdMapVal)] String
--         mDefResult' =  case mDefResult of
--                             t@(Right errlog) -> t
--                             (Left (mName, mDef)) -> Left [(mName, Left mDef)]
--         aDefResult' :: [Either ]
--     in
--         case (mDefResult, aDefResult) of
--             (t@(Right errmsg), _) -> t
--             (_, t@(Right errmsg)) -> t
--             ((Left mDefMap), (Right aDefMap)) -> Left 
--     -- (superClassName', Map String TClassAttrVarDef, Map String TClassMethodDef)
-- transParserRootElem (PRootMethodDef (attrAccessMod, isStatic, varDef)) =
-- 
-- 
-- transParserClassMethodDefs :: [PClassMethodDef] -> [Either (String, TClassMethodDef) String]
-- transParserClassAttrDefs :: [PClassAttrDef] -> [Either (String, TClassAttrDef) String]
