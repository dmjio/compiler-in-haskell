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
    "return"                            {tok (\p s -> Return p)}
    $digit+                             {tok (\p s -> Int p (read s))}
    [$alpha \_] [$alpha $digit \_]*     {tok (\p s -> Var p s)}
    [\=\+\-\*\/\_\(\)\{\}\;]            {tok (\p s -> Sym p (head s))}

{
tok f p s = f p s

data Token = Return AlexPosn
           | Int    AlexPosn Int
           | Sym    AlexPosn Char
           | Var    AlexPosn String
             deriving (Eq, Show)

token_posn (Return p)   = p
token_posn (Int p _)    = p
token_posn (Sym p _)    = p
token_posn (Var p _)    = p
}
