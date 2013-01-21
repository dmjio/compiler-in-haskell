{
module Scanner.DLCScanner
    (Token(..), AlexPosn(..), alexScanTokens, token_posn)
where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                             ;
    "return"                            {tok (\p s -> KW_Return p)}
    "int"                               {tok (\p s -> KW_Int p)}
    "void"                              {tok (\p s -> KW_Void p)}
    $digit+                             {tok (\p s -> Int p (read s))}
    [$alpha \_] [$alpha $digit \_]*     {tok (\p s -> Var p s)}
    [\=\+\-\*\/\(\)\{\}\;\,]            {tok (\p s -> Sym p (head s))}

{
tok f p s = f p s

data Token = KW_Return      AlexPosn
           | KW_Int         AlexPosn
           | KW_Void        AlexPosn
           | Int            AlexPosn Int
           | Sym            AlexPosn Char
           | Var            AlexPosn String
             deriving (Eq, Show)

token_posn (KW_Return p)  = p
token_posn (KW_Int p)     = p
token_posn (KW_Void p)     = p
token_posn (Int p _)      = p
token_posn (Sym p _)      = p
token_posn (Var p _)      = p
}
