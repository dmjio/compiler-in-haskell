module AST
    where

import Data.Either

type SourceCode = [Func]
type Func = (Type, String, [(Type, String)], [Either Stmt Expr])
data Type = Int_t
          | Void_t
            deriving (Eq, Show)
data Stmt = ReturnStmt Expr
          | DefStmt Type String
          | AssignStmt String Expr
            deriving (Eq, Show)
data Expr = FunCallExpr String [Expr]
          | AddExpr Expr Expr
          | SubExpr Expr Expr
          | MulExpr Expr Expr
          | DivExpr Expr Expr
          | NegExpr Expr
          | IntExpr Int
          | VarExpr String
            deriving (Eq, Show)

typeToSize :: Type -> Int
typeToSize Int_t = 4
typeToSize Void_t = 0
