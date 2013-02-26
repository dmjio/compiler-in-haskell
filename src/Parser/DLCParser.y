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
%left "." "[" "]" FCALL "++" "--"

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

ClassAttrDef : ClassAccessModifier kw_static VarDef ";" { ($1,  True, $3) }
             | ClassAccessModifier           VarDef ";" { ($1, False, $2) }

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

Sentence : Expr ";" { Right $1 }
         | Stmt     { Left  $1 }

VarDef : Type VarDefItemList      { ($1, $2) }

VarDefItemList : VarDefItem "," VarDefItemList { $1:$3 }
               | VarDefItem                    { [$1]  }

VarDefItem : var "=" Expr         { ($1, Just $3) }
           | var                  { ($1, Nothing) }

Type : kw_void { PVoid }
     | ValType { $1 }

ValType : kw_int ArrDepth         { makeArrType PInt $2 }
        | kw_byte ArrDepth        { makeArrType PByte $2 }
        | kw_bool ArrDepth        { makeArrType PBool $2 }
        | var                     { makeArrType (PObjClass $1) 0 }
        | Expr "[" "]" ArrDepth   { makeArrType (PObjClass $ extractClassName $1) ($4 + 1) }

ArrDepth : "[" "]" ArrDepth { 1 + $3 }
         | {- empty -}      { 0 }

BasicValType : kw_int   { PInt }
             | kw_byte  { PByte }
             | kw_bool  { PBool }
             | var      { PObjClass $1 }

Stmt : VarDef ";"                                                 { PStmtVarDef $1 }
     | kw_print "(" ExprList ")" ";"                              { PStmtPrint  $3 }
     | kw_print "(" ")" ";"                                       { PStmtPrint  [] }
     | kw_if "(" Expr ")" "{" SentenceList "}"                    { PStmtIf $3 $6 [] }
     | kw_if "(" Expr ")" "{" SentenceList "}" kw_else "{" SentenceList "}"
                                                                  { PStmtIf $3 $6 $10 }
     | kw_for "(" Expr ";" Expr ";" Expr ")" "{" SentenceList "}" 
                                                              { PStmtFor (Right $3) $5 $7 $10 }
     | kw_for "(" VarDef ";" Expr ";" Expr ")" "{" SentenceList "}" 
                                                  { PStmtFor (Left $ PStmtVarDef $3) $5 $7 $10}
     | kw_while "(" Expr ")" "{" SentenceList "}"                 { PStmtWhile $3 $6 }
     | kw_do "{" SentenceList "}" kw_while "(" Expr ")" ";"       { PStmtDoWhile $3 $7 }
     | kw_return Expr ";"                                         { PStmtReturn $2 }

ExprList : Expr "," ExprList { $1:$3 }
         | Expr              { [$1]  }

CompoundExpr :: { PExpr }
             : Expr "." var "(" ExprList ")"  %prec FCALL  { et (PExprFunCall (Just $1) $3 $5)}
             | Expr "." var "(" ")"           %prec FCALL  { et (PExprFunCall (Just $1) $3 [])}
             | var "(" ExprList ")"           %prec FCALL  { et (PExprFunCall Nothing $1 $3)}
             | var "(" ")"                    %prec FCALL  { et (PExprFunCall Nothing $1 [])}
             | "(" CompoundExpr ")"                        { $2 }
             | Expr "+" Expr                               { et (PExprAdd $1 $3)}
             | Expr "-" Expr                               { et (PExprMin $1 $3)}
             | Expr "*" Expr                               { et (PExprMul $1 $3)}
             | Expr "/" Expr                               { et (PExprDiv $1 $3)}
             | "-" Expr                         %prec NEG  { et (PExprNeg $2)}
             | Expr kw_and Expr                            { et (PExprAnd $1 $3)}
             | Expr kw_or  Expr                            { et (PExprOr  $1 $3)}
             | kw_not Expr                                 { et (PExprNot $2)}
             | "++" Expr                 %prec PREFIX_INC  { et (PExprIncV $2)}
             | "--" Expr                 %prec PREFIX_DEC  { et (PExprDecV $2)}
             | Expr "++"                                   { et (PExprVInc $1)}
             | Expr "--"                                   { et (PExprVDec $1)}
             | Expr "+=" Expr                              { et (PExprIncBy $1 $3)}
             | Expr "-=" Expr                              { et (PExprDecBy $1 $3)}
             | Expr "==" Expr                              { et (PExprEq $1 $3)}
             | Expr "!=" Expr                              { et (PExprNeq $1 $3)}
             | Expr "<=" Expr                              { et (PExprLeq $1 $3)}
             | Expr ">=" Expr                              { et (PExprGeq $1 $3)}
             | Expr "<" Expr                               { et (PExprLe $1 $3)}
             | Expr ">" Expr                               { et (PExprGe $1 $3)}
             | Expr "[" Expr "]"                           { et (PExprArrAccess $1 $3) }
             | Expr "." var                                { et (PExprDotAccess $1 $3)}
             | "(" ValType ")" Expr           %prec TCAST  { et (PExprConvType $2 $4)}
             | Expr "=" Expr                               { et (PExprAssign $1 $3)}
             | kw_new var "(" ")"                          { et (PExprNewObj $2)}
             | kw_new BasicValType NewArrArgs              { et (PExprNewArr $2 $3)}

Expr :: { PExpr }
     : bool                                        { et (PExprBool $1)}
     | var                                         { et (PExprVar $1)}
     | kw_self                                     { et (PExprVar "self")}
     | kw_super                                    { et (PExprVar "super")}
     | str                                         { et (PExprStr $1)}
     | int                                         { et (PExprInt $1)}
     | char                                        { et (PExprChar $1)}
     | kw_null                                     { et (PExprNull)}
     | CompoundExpr                                { $1 }

NewArrArgs : "[" Expr "]"            { [$2] }
           | NewArrArgs "[" Expr "]" { $1 ++ [$3] } 

{
extractClassName :: PExpr -> String
-- extractClassName (_, (PExprVar s)) = s
extractClassName (PExprVar s) = s
-- -- for the rest patterns, let it fail...
-- -- (we need to come up with a way to report the error though)

makeArrType :: PType -> Int -> PType
makeArrType t 0 = t
makeArrType t n = makeArrType (PArray t) (n-1)

run_parser :: String -> PRoot
run_parser = calc . alexScanTokens

-- et :: PExpr -> PTExpr
-- et expr = (PNull, expr)
et :: a -> a
et = id

happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c ++ ", token <" ++ show tk ++ ">"
			where
			AlexPn _ l c = token_posn tk
}
