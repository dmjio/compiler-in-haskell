{
module Scanner.DLCScanner
    (Token(..), AlexPosn(..), alexScanTokens, token_posn, token_print)
where

import Debug.Trace(trace)
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                             ;
    \/\/ .*                             ;
    "return"                            {(\p s -> KW_Return p)}
    "int"                               {(\p s -> KW_Int p)}
    "int32"                             {(\p s -> KW_Int32 p)}
    "byte"                              {(\p s -> KW_Byte p)}
    "void"                              {(\p s -> KW_Void p)}
    "class"                             {(\p s -> KW_Class p)}
    "if"                                {(\p s -> KW_If p)}
    "do"                                {(\p s -> KW_Do p)}
    "while"                             {(\p s -> KW_While p)}
    $digit+                             {(\p s -> Int p (read s))}
    [$alpha \_] [$alpha $digit \_]*     {(\p s -> Var p s)}
    "++"                                {(\p s -> Sym p s)}
    "+="                                {(\p s -> Sym p s)}
    "--"                                {(\p s -> Sym p s)}
    "-="                                {(\p s -> Sym p s)}
    "=="                                {(\p s -> Sym p s)}
    "<="                                {(\p s -> Sym p s)}
    ">="                                {(\p s -> Sym p s)}
    [\+\-\*\/\=\<\>\(\)\{\}\;\,\.]      {(\p s -> Sym p s)}
    \' \\? . \'                         {(\p s -> DChar p $ unescape s)}
{

data Token = KW_Return      AlexPosn
           | KW_Int         AlexPosn
           | KW_Int32       AlexPosn
           | KW_Byte        AlexPosn
           | KW_Void        AlexPosn
           | KW_Class       AlexPosn
           | KW_If          AlexPosn
           | KW_Do          AlexPosn
           | KW_While       AlexPosn
           | Int            AlexPosn Int
           | Var            AlexPosn String
           | Sym            AlexPosn String -- ";", "(", "{", "++", "-", ...
           | DChar          AlexPosn Char   -- '\n', '\t', '0'...
             deriving (Eq, Show)

-- just for fun... I miss Alan so much... :(
unescape :: String -> Char
unescape = head . (read :: String -> String ) . ("\"" ++) . (++ "\"") . tail . init

token_print :: [Token] -> IO ()
token_print [] = return ()
token_print (x:xs) =
    do
        p_token x
        token_print xs
    where
        p_token :: Token -> IO ()
        p_token (KW_Return p)  = pp p $ "KW_Return"
        p_token (KW_Int    p)  = pp p $ "KW_Int"
        p_token (KW_Int32  p)  = pp p $ "KW_Int32"
        p_token (KW_Byte   p)  = pp p $ "KW_Byte"
        p_token (KW_Void   p)  = pp p $ "KW_Void"
        p_token (KW_Class  p)  = pp p $ "KW_Class"
        p_token (KW_If     p)  = pp p $ "KW_If"
        p_token (KW_Do     p)  = pp p $ "KW_Do"
        p_token (KW_While  p)  = pp p $ "KW_While"
        p_token (Int     p v)  = pp p $ "Int      " ++ (show v)
        p_token (Var     p v)  = pp p $ "Var      " ++ (show v)
        p_token (Sym     p v)  = pp p $ "Sym      " ++ (show v)
        p_token (DChar   p v)  = pp p $ "DChar    " ++ (show v)

        pp :: AlexPosn -> String -> IO ()
        pp (AlexPn a b c) s =
            putStrLn $ "(" ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++ ") " ++ s


token_posn (KW_Return p)  = p
token_posn (KW_Int    p)  = p
token_posn (KW_Int32  p)  = p
token_posn (KW_Byte   p)  = p
token_posn (KW_Void   p)  = p
token_posn (KW_Class  p)  = p
token_posn (KW_If     p)  = p
token_posn (KW_Do     p)  = p
token_posn (KW_While  p)  = p
token_posn (Int   p _)    = p
token_posn (Var   p _)    = p
token_posn (Sym   p _)    = p
token_posn (DChar p _)    = p
}
