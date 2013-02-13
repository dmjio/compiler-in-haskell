{
module Parser.DLCParser (run_parser)
    where

import Scanner.DLCScanner
import Parser.ParserAST

-- FIXME: &, |, ^, ~
}

%name calc
%tokentype {Token}

%token
    kw_return     { KW_Return     _      }
    kw_int        { KW_Int        _      }
    kw_int32      { KW_Int32      _      }
    kw_byte       { KW_Byte       _      }
    kw_bool       { KW_Bool       _      }
    kw_true       { KW_True       _      }
    kw_false      { KW_False      _      }
    kw_void       { KW_Void       _      }
    kw_class      { KW_Class      _      }
    kw_public     { KW_Public     _      }
    kw_protected  { KW_Protected  _      }
    kw_self       { KW_Self       _      }
    kw_super      { KW_Super      _      }
    kw_new        { KW_New        _      }
    kw_extends    { KW_Extends    _      }
    kw_if         { KW_If         _      }
    kw_else       { KW_Else       _      }
    kw_for        { KW_For        _      }
    kw_do         { KW_Do         _      }
    kw_while      { KW_While      _      }
    kw_print      { KW_Print      _      }
    kw_and        { KW_And        _      }
    kw_or         { KW_Or         _      }
    kw_not        { KW_Not        _      }
    kw_null       { KW_Null       _      }
    int           { Int           _ $$   }
    var           { Var           _ $$   }
    "++"          { Sym           _ "++" }
    "+="          { Sym           _ "+=" }
    "--"          { Sym           _ "--" }
    "-="          { Sym           _ "-=" }
    "=="          { Sym           _ "==" }
    "<="          { Sym           _ "<=" }
    ">="          { Sym           _ ">=" }
    "+"           { Sym           _ "+"  }
    "-"           { Sym           _ "-"  }
    "*"           { Sym           _ "*"  }
    "/"           { Sym           _ "/"  }
    "="           { Sym           _ "="  }
    "<"           { Sym           _ "<"  }
    ">"           { Sym           _ ">"  }
    "("           { Sym           _ "("  }
    ")"           { Sym           _ ")"  }
    "{"           { Sym           _ "{"  }
    "}"           { Sym           _ "}"  }
    "["           { Sym           _ "["  }
    "]"           { Sym           _ "]"  }
    ";"           { Sym           _ ";"  }
    ","           { Sym           _ ","  }
    "."           { Sym           _ "."  }
    char          { DChar         _ $$   }
    str           { DStr          _ $$   }

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

FParamList : Type var                   { [($1, $2)]  }
           | Type var ',' FParamList    { ($1, $2):$4 }

FuncBody : Stmt ';' FuncBody    { (Left $1):$3  }
         | Expr ';' FuncBody    { (Right $1):$3 }
         | {- empty -}          { [] }

Stmt : kw_return Expr           { ReturnStmt $2    }
     | Type var                 { DefStmt $1 $2    }
     | var '=' Expr             { AssignStmt $1 $3 }

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
run_parser :: String -> SourceCode
run_parser = calc . alexScanTokens

happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c
			where
			AlexPn _ l c = token_posn tk
}
