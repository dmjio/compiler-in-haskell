module Parser.ParserAST
    where

type PRoot = [PRootElem]
data PRootElem = PRootClassDef  PClassDef
               | PRootMethodDef PMethodDef
                 deriving (Show, Eq)

--                className superClassName
type PClassDef = (String,   Maybe String,  [Either PClassMethodDef PClassAttrDef])

--                      public/protected/none       T: static
type PClassMethodDef = (Maybe PClassAccessModifier, Bool,     PMethodDef)
type PClassAttrDef =   (Maybe PClassAccessModifier, Bool,     PVarDef)

type PMethodDef = (PType, String, [(PType, String)], [PSentence])
type PVarDef = (PType, [(String, Maybe PExpr)])

data PClassAccessModifier = PPublic | PProtected | PPrivate deriving (Eq, Show)

data PType = PVoid | PValType deriving (Eq, Show)
data PBasicValType = PInt | PInt32 | PByte | PBool | PObjClass String deriving (Eq, Show)
data PValType = PBasicValType | PArray PValType deriving (Eq, Show)

type PSentence = Either PStmt PExpr

data PStmt = PStmtVarDef PVarDef
           | PStmtPrint [PExpr]
           | PStmtIf PExpr PSentence (Maybe PSentence)
           | PStmtFor PSentence PExpr PExpr [PSentence]
           | PStmtWhile PExpr [PSentence]
           | PStmtDoWhile [PSentence] PExpr
           | PStmtReturn PExpr
             deriving (Eq, Show)
data PExpr = PExprList [PExpr] -- combined expr: "a++, a=1, k[10].x=x+2" (could be empty)
           | PExprFunCall (Maybe PExpr) String [PExpr]
           | PExprAdd PExpr PExpr
           | PExprMin PExpr PExpr
           | PExprMul PExpr PExpr
           | PExprDiv PExpr PExpr
           | PExprAnd PExpr PExpr
           | PExprOr PExpr PExpr
           | PExprNot PExpr
           | PExprInc PExpr
           | PExprDec PExpr
           | PExprIncBy PExpr PExpr
           | PExprDecBy PExpr PExpr
           | PExprLeq PExpr PExpr
           | PExprGeq PExpr PExpr
           | PExprLe PExpr PExpr
           | PExprGe PExpr PExpr
           | PExprArrAccess PExpr PExpr
           | PExprDotAccess PExpr String
           | PExprVar String
           | PExprInt Int
           | PExprNewObj String
           | PExprNewArr PBasicValType [Int]
             deriving (Eq, Show)
