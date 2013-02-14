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
    kw_byte       { KW_Byte       _      }
    kw_bool       { KW_Bool       _      }
    kw_void       { KW_Void       _      }
    kw_class      { KW_Class      _      }
    kw_public     { KW_Public     _      }
    kw_protected  { KW_Protected  _      }
    kw_private    { KW_Private    _      }
    kw_static     { KW_Static     _      }
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
    bool          { Boolean       _ $$   }
    int           { Int           _ $$   }
    var           { Var           _ $$   }
    "++"          { Sym           _ "++" }
    "+="          { Sym           _ "+=" }
    "--"          { Sym           _ "--" }
    "-="          { Sym           _ "-=" }
    "=="          { Sym           _ "==" }
    "!="          { Sym           _ "!=" }
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

%left ","
%right "=" "+=" "-="
%left kw_or
%left kw_and
%left "==" "!="
%left ">" "<" ">=" "<="
%left "+" "-"
%left "*" "/"
%right kw_not PREFIX_INC PREFIX_DEC NEG TCAST kw_new
%left "." FCALL ARR_SUB SUFFIX_INC SUFFIX_DEC

%%

Root : RootElem Root    { $1:$2 }
     | {- empty -}      { [] }

RootElem : ClassDef   { PRootClassDef  $1 }
         | MethodDef  { PRootMethodDef $1 }

ClassDef : kw_class var kw_extends var "{" ClassMethodOrClassAttrDefList "}"
                      { ($2, Just $4, $6) }
         | kw_class var                "{" ClassMethodOrClassAttrDefList "}"
                      { ($2, Nothing, $4) }

ClassMethodOrClassAttrDefList : ClassMethodOrClassAttrDef ClassMethodOrClassAttrDefList { $1:$2 }
                              | {- empty -}                                             { []    }

ClassMethodOrClassAttrDef : ClassMethodDef          { Left  $1 }
                          | ClassAttrDef            { Right $1 }

ClassMethodDef : ClassAccessModifier kw_static MethodDef { ($1,  True, $3) }
               | ClassAccessModifier           MethodDef { ($1, False, $2) }

ClassAttrDef : ClassAccessModifier kw_static VarDef { ($1,  True, $3) }
             | ClassAccessModifier           VarDef { ($1, False, $2) }

ClassAccessModifier : kw_public    { Just PPublic }
                    | kw_protected { Just PProtected }
                    | kw_private   { Just PPrivate }
                    | {- empty -}  { Nothing }

MethodDef : Type var "(" MethodDefArgList ")" "{" SentenceList "}"  { ($1, $2, $4, $7) }
          | Type var "("                  ")" "{" SentenceList "}"  { ($1, $2, [], $6) }

MethodDefArgList : Type var "," MethodDefArgList   { ($1, $2):$4 }
                 | Type var                        { [($1, $2)]  }

SentenceList : Sentence SentenceList { $1:$2 }
             | {- empty -}           { [] }

Sentence : Stmt     { Left  $1 }
         | Expr ";" { Right $1 }

VarDef : Type VarDefItemList ";"  { ($1, $2) }

VarDefItemList : VarDefItem "," VarDefItemList { $1:$3 }
               | VarDefItem                    { [$1]  }

VarDefItem : var "=" Expr         { ($1, Just $3) }
           | var                  { ($1, Nothing) }

Type : kw_void { PVoid }
     | ValType { $1 }

ValType : kw_int          { PInt }
        | kw_byte         { PByte }
        | kw_bool         { PBool }
        | var             { PObjClass $1 }
        | var "[" "]"     { PArray (PObjClass $1) }
        | ValType "[" "]" { PArray $1 }

BasicValType : kw_int   { PInt }
             | kw_byte  { PByte }
             | kw_bool  { PBool }
             | var      { PObjClass $1 }

ControlStmtBody : Sentence             { [$1]   }
                | "{" SentenceList "}" { $2   }
                | "{" Sentence "}"     { [$2] }
                | ";"                  { [] }

Stmt : VarDef ";"                                                 { PStmtVarDef $1 }
     | kw_print "(" ExprList ")" ";"                              { PStmtPrint  $3 }
     | kw_print "(" ")" ";"                                       { PStmtPrint  [] }
     | kw_if "(" Expr ")" ControlStmtBody                         { PStmtIf $3 $5 [] }
     | kw_if "(" Expr ")" ControlStmtBody kw_else ControlStmtBody { PStmtIf $3 $5 $7 }
     | kw_for "(" Expr ";" Expr ";" Expr ")" ControlStmtBody      { PStmtFor (Right $3) $5 $7 $9 }
     | kw_for "(" VarDef ";" Expr ";" Expr ")" ControlStmtBody
                                                   { PStmtFor (Left $ PStmtVarDef $3) $5 $7 $9}
     | kw_while "(" Expr ")" ControlStmtBody                      { PStmtWhile $3 $5 }
     | kw_do ControlStmtBody kw_while "(" Expr ")" ";"            { PStmtDoWhile $2 $5 }
     | kw_return Expr ";"                                         { PStmtReturn $2 }

ExprList : Expr "," ExprList { $1:$3 }
         | Expr              { [$1]  }

CompoundExpr : Expr "." var "(" ExprList ")"  %prec FCALL  { PExprFunCall (Just $1) $3 $5 }
             | Expr "." var "(" ")"           %prec FCALL  { PExprFunCall (Just $1) $3 [] }
             | var "(" ExprList ")"           %prec FCALL  { PExprFunCall Nothing $1 $3 }
             | var "(" ")"                    %prec FCALL  { PExprFunCall Nothing $1 [] }
             | "(" CompoundExpr ")"                        { $2 }
             | Expr "+" Expr                               { PExprAdd $1 $3 }
             | Expr "-" Expr                               { PExprMin $1 $3 }
             | Expr "*" Expr                               { PExprMul $1 $3 }
             | Expr "/" Expr                               { PExprDiv $1 $3 }
             | "-" Expr                         %prec NEG  { PExprNeg $2 }
             | Expr kw_and Expr                            { PExprAnd $1 $3 }
             | Expr kw_or  Expr                            { PExprOr  $1 $3 }
             | kw_not Expr                                 { PExprNot $2 }
             | "++" Expr                 %prec PREFIX_INC  { PExprIncV $2 }
             | "--" Expr                 %prec PREFIX_DEC  { PExprDecV $2 }
             | Expr "++"                 %prec SUFFIX_INC  { PExprVInc $1 }
             | Expr "--"                 %prec SUFFIX_DEC  { PExprVDec $1 }
             | Expr "+=" Expr                              { PExprIncBy $1 $3 }
             | Expr "-=" Expr                              { PExprDecBy $1 $3 }
             | Expr "==" Expr                              { PExprEq $1 $3 }
             | Expr "!=" Expr                              { PExprNeq $1 $3 }
             | Expr "<=" Expr                              { PExprLeq $1 $3 }
             | Expr ">=" Expr                              { PExprGeq $1 $3 }
             | Expr "<" Expr                               { PExprLe $1 $3 }
             | Expr ">" Expr                               { PExprGe $1 $3 }
             | Expr "[" Expr "]"            %prec ARR_SUB  { PExprArrAccess $1 $3 }
             | Expr "." var                                { PExprDotAccess $1 $3 }
             | "(" ValType ")" Expr           %prec TCAST  { PExprConvType $2 $4 }
             | Expr "." var "=" Expr                       { PExprAssign (Just $1) $3 $5 }
             | var "=" Expr                                { PExprAssign Nothing $1 $3 }
             | kw_new var "(" ")"                          { PExprNewObj $2 }
             | kw_new BasicValType NewArrArgs              { PExprNewArr $2 $3 }

Expr : bool                                        { PExprBool $1 }
     | var                                         { PExprVar $1 }
     | kw_self                                     { PExprVar "self" }
     | kw_super                                    { PExprVar "super" }
     | str                                         { PExprStr $1 }
     | int                                         { PExprInt $1 }
     | char                                        { PExprChar $1 }
     | kw_null                                     { PExprNull }
     | CompoundExpr                                { $1 }

NewArrArgs : "[" int "]" NewArrArgs { $2:$4 }
           | "[" int "]"            { [$2] }
{
run_parser :: String -> PRoot
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
