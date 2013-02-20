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
type PVarDef = (PType, [(String, Maybe PTExpr)])

data PClassAccessModifier = PPublic | PProtected | PPrivate deriving (Eq, Show)

data PType = PVoid | PInt | PInt32 | PByte | PBool | PObjClass String | PArray PType
           | PNull -- PNull is used as the initial expression type placeholder
             deriving (Eq, Show)

type PSentence = Either PStmt PTExpr

data PStmt = PStmtVarDef PVarDef
           | PStmtPrint [PTExpr]
           | PStmtIf PTExpr [PSentence] [PSentence]
           | PStmtFor PSentence PTExpr PTExpr [PSentence]
           | PStmtWhile PTExpr [PSentence]
           | PStmtDoWhile [PSentence] PTExpr
           | PStmtReturn PTExpr
             deriving (Eq, Show)

type PTExpr = (PType, PExpr)

data PExpr = PExprList [PTExpr] -- a++, a=1, k[10].x=x+2 (could be empty)
           | PExprFunCall (Maybe PTExpr) String [PTExpr] -- point[8].dump()
           | PExprAdd PTExpr PTExpr   -- a + b
           | PExprMin PTExpr PTExpr   -- a - b
           | PExprMul PTExpr PTExpr   -- a * b
           | PExprDiv PTExpr PTExpr   -- a / b
           | PExprNeg PTExpr         -- -a
           | PExprAnd PTExpr PTExpr   -- a and b  <--  a && b
           | PExprOr PTExpr PTExpr    -- a or b   <--  a || b
           | PExprNot PTExpr         -- not a    <--  !a
           | PExprIncV PTExpr        -- ++i
           | PExprDecV PTExpr        -- --i
           | PExprVInc PTExpr        -- i++
           | PExprVDec PTExpr        -- i--
           | PExprIncBy PTExpr PTExpr -- i += n
           | PExprDecBy PTExpr PTExpr -- i -= n
           | PExprEq PTExpr PTExpr    -- i == n
           | PExprNeq PTExpr PTExpr   -- i != n
           | PExprLeq PTExpr PTExpr   -- i <= n
           | PExprGeq PTExpr PTExpr   -- i >= n
           | PExprLe PTExpr PTExpr    -- i < n
           | PExprGe PTExpr PTExpr    -- i > n
           | PExprArrAccess PTExpr PTExpr     -- a[expr]
           | PExprDotAccess PTExpr String    -- expr.hello
           | PExprBool Bool     -- True/False
           | PExprVar String    -- c
           | PExprInt Int       -- 12
           | PExprStr String    -- "Hello\n"
           | PExprChar Int      -- '\n'
           | PExprNull          -- null
           | PExprConvType PType PTExpr -- (Object)s
           | PExprAssign PTExpr PTExpr -- i = 5; i[10] = 5; i.v = 5; NOT i.getX() = 5;
           | PExprNewObj String              -- new Hi()
           | PExprNewArr PType [PTExpr] -- new int[35]
             deriving (Eq, Show)
