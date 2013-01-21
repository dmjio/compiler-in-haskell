{
module Parser.DLCParser
    (Type(..), Stmt(..), Expr(..), run_parser)
where

import Scanner.DLCScanner
import Data.Either
}

%name calc
%tokentype {Token}

%token
    kw_int     { KW_Int    _     }
    kw_void    { KW_Void   _     }
    kw_return  { KW_Return _     }
    int        { Int       _ $$  }
    var        { Var       _ $$  }
--  '='        { Sym       _ '=' }
    '+'        { Sym       _ '+' }
    '-'        { Sym       _ '-' }
    '*'        { Sym       _ '*' }
    '/'        { Sym       _ '/' }
    '('        { Sym       _ '(' }
    ')'        { Sym       _ ')' }
    '{'        { Sym       _ '{' }
    '}'        { Sym       _ '}' }
    ';'        { Sym       _ ';' }
    ','        { Sym       _ ',' }

%left '+' '-'
%left '*' '/'
%left NEG

%%

Root : Func Root        { $1:$2 }
     | {- empty -}      { [] }

Func : Type var '(' FParamList ')' '{' FuncBody '}'  { ($1, $2, $4, $7) }
     | Type var '('            ')' '{' FuncBody '}'  { ($1, $2, [], $6) }

Type : kw_int           { Int_t  }
     | kw_void          { Void_t }

FParamList : Type var                   { [($1, $2)] }
           | Type var ',' FParamList    { ($1, $2):$4    }

FuncBody : Stmt ';' FuncBody    { (Left $1):$3  }
         | Expr ';' FuncBody    { (Right $1):$3 }
         | {- empty -}          { [] }

Stmt : kw_return Expr           { ReturnStmt $2 }

Expr : '(' Expr ')'             { $2 }
     | var '(' ')'              { FunCallExpr $1 [] }
     | var '(' ParamList ')'    { FunCallExpr $1 $3 }
     | Expr '+' Expr            { AddExpr $1 $3 }
     | Expr '-' Expr            { SubExpr $1 $3 }
     | Expr '*' Expr            { MulExpr $1 $3 }
     | Expr '/' Expr            { DivExpr $1 $3 }
     | '-' Expr      %prec NEG  { NegExpr $2 }
     | int                      { IntExpr $1 }
     | var                      { VarExpr $1 }

ParamList : Expr                { [$1]  }
          | Expr ',' ParamList  { $1:$3 }

{
type SourceCode = [Func]
type Func = (Type, String, [(Type, String)], [Either Stmt Expr])
data Type = Int_t
          | Void_t
            deriving (Eq, Show)
data Stmt = ReturnStmt Expr
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

run_parser :: String -> SourceCode
run_parser = calc . alexScanTokens

-- dafuq?
happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c
			where
			AlexPn _ l c = token_posn tk
}
