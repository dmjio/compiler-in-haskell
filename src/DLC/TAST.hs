module DLC.TAST where -- export everything!

import Data.Map

type TransResult = (Map String TClassDef, Map String TMethodDef)

--     class      C       extends B { public int a;      public int f(){} }
type TClassDef = (String, String,    [TClassAttrDef],   [TClassMethodDef])

--                         public          (static)   int i;
type TClassAttrDef = (TClassAccessModifier, Bool,     TVarDef)

--               i       int =  0;
type TVarDef = (String, TType, TExpr)

--                          public            (static)   f() {}
type TClassMethodDef = (TClassAccessModifier, Bool,    TMethodDef)

--                  f       int     (a       bool)    {...}
type TMethodDef = (String, TType, [(String, TType)], [TBodyStmt])

data TClassAccessModifier = TPublic | TProtected | TPrivate deriving (Eq, Show)
data TBodyStmt = TBSStmt TStmt | TBSExpr TExpr deriving (Eq, Show)
data TType = TVoid | TInt | TInt32 | TByte | TBool | TClass String 
           | TUnknown -- for C functions, and null. null will just be treated as 0.
           | TArray Int TType -- TArray 1 TBool: "TBool[]", the Int value should be >= 1.
             deriving (Eq, Show, Ord)
data TStmt = TStmtVarDef TVarDef
           | TStmtPrint TExpr
           | TStmtIf TExpr [TBodyStmt] [TBodyStmt]
           | TStmtFor (Either TExpr (TType, [(String, TExpr)])) TExpr TExpr [TBodyStmt]
           | TStmtWhile TExpr [TBodyStmt]
           | TStmtDoWhile [TBodyStmt] TExpr
           | TStmtReturn TExpr
             deriving (Eq, Show)
-- caution: definition of TExpr is made evilly by "copy-paste-text_replacing".
--          it's a copy of PExpr, but not exactly the same. (I changed PType to TType.)
data TExpr = TExprFunCall (Maybe TExpr) String [TExpr] -- point[8].dump()
           | TExprAdd TExpr TExpr   -- a + b
           | TExprMin TExpr TExpr   -- a - b
           | TExprMul TExpr TExpr   -- a * b
           | TExprDiv TExpr TExpr   -- a / b
           | TExprNeg TExpr         -- -a
           | TExprAnd TExpr TExpr   -- a and b  <--  a && b
           | TExprOr TExpr TExpr    -- a or b   <--  a || b
           | TExprNot TExpr         -- not a    <--  !a
           | TExprIncV TExpr        -- ++i
           | TExprDecV TExpr        -- --i
           | TExprVInc TExpr        -- i++
           | TExprVDec TExpr        -- i--
           | TExprIncBy TExpr TExpr -- i += n
           | TExprDecBy TExpr TExpr -- i -= n
           | TExprEq TExpr TExpr    -- i == n
           | TExprNeq TExpr TExpr   -- i != n
           | TExprLeq TExpr TExpr   -- i <= n
           | TExprGeq TExpr TExpr   -- i >= n
           | TExprLe TExpr TExpr    -- i < n     -- FIXME: not le... should be l or less...
           | TExprGe TExpr TExpr    -- i > n     -- FIXME
           | TExprArrAccess TExpr TExpr     -- a[expr]
           | TExprDotAccess TExpr String    -- expr.hello
           | TExprBool Bool     -- True/False
           | TExprVar String    -- c
           | TExprInt Int       -- 12
           | TExprStr String    -- "Hello\n"
           | TExprChar Int      -- '\n'
           | TExprNull          -- null
           | TExprConvType TType TExpr -- (Object)s
           | TExprAssign TExpr TExpr -- i = 5; i[10] = 5; i.v = 5; NOT i.getX() = 5;
           | TExprNewObj String              -- new Hi()
           | TExprNewArr TType [TExpr] -- new int[35]
             deriving (Eq, Show)
