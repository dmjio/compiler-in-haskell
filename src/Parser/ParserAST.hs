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
           | PStmtPrint PExpr -- PExprList / single expr
           | PStmtIf PExpr [PSentence] [PSentence]
           | PStmtFor PSentence PExpr PExpr [PSentence]
           | PStmtWhile PExpr [PSentence]
           | PStmtDoWhile [PSentence] PExpr
           | PStmtReturn PExpr
             deriving (Eq, Show)
data PExpr = PExprList [PExpr] -- a++, a=1, k[10].x=x+2 (could be empty)
           | PExprFunCall (Maybe PExpr) String [PExpr] -- point[8].dump()
           | PExprAdd PExpr PExpr   -- a + b
           | PExprMin PExpr PExpr   -- a - b
           | PExprMul PExpr PExpr   -- a * b
           | PExprDiv PExpr PExpr   -- a / b
           | PExprNeg PExpr         -- -a
           | PExprAnd PExpr PExpr   -- a and b <= a && b
           | PExprOr PExpr PExpr    -- a or b  <= a || b
           | PExprNot PExpr         -- not a   <= !a
           | PExprIncV PExpr        -- ++i
           | PExprDecV PExpr        -- --i
           | PExprVInc PExpr        -- i++
           | PExprVDec PExpr        -- i--
           | PExprIncBy PExpr PExpr -- i += n
           | PExprDecBy PExpr PExpr -- i -= n
           | PExprEq PExpr PExpr    -- i == n
           | PExprNeq PExpr PExpr   -- i != n
           | PExprLeq PExpr PExpr   -- i <= n
           | PExprGeq PExpr PExpr   -- i >= n
           | PExprLe PExpr PExpr    -- i < n
           | PExprGe PExpr PExpr    -- i > n
           | PExprArrAccess PExpr PExpr     -- a[expr]
           | PExprDotAccess PExpr String    -- expr.hello
           | PExprBool Bool     -- true/false
           | PExprVar String    -- c
           | PExprInt Int       -- 12
           | PExprStr String    -- "Hello\n"
           | PExprChar Int      -- '\n'
           | PExprNewObj String              -- new Hi()
           | PExprNewArr PBasicValType [Int] -- new int[35]
             deriving (Eq, Show)
