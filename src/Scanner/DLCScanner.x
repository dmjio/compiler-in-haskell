{
module Scanner.DLCScanner
    (Token(..), AlexPosn(..), alexScanTokens, token_posn, token_print)
where

import Data.Char(ord)
import Debug.Trace(trace)
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
@esc_char = (\\t|\\n|\\b|\\0|\\\\)
$reg_char = [^\\\"]

tokens :-
    $white+                             ;
    \/\/.*                              ;
    "return"                            {(\p s -> KW_Return    p)}
    "int"                               {(\p s -> KW_Int       p)}
    "int32"                             {(\p s -> KW_Int32     p)}
    "byte"                              {(\p s -> KW_Byte      p)}
    "bool"                              {(\p s -> KW_Bool      p)}
    "true"                              {(\p s -> KW_True      p)}
    "false"                             {(\p s -> KW_False     p)}
    "void"                              {(\p s -> KW_Void      p)}
    "class"                             {(\p s -> KW_Class     p)}
    "public"                            {(\p s -> KW_Public    p)}
    "protected"                         {(\p s -> KW_Protected p)}
    "self"                              {(\p s -> KW_Self      p)}
    "super"                             {(\p s -> KW_Super     p)}
    "new"                               {(\p s -> KW_New       p)}
    "extends"                           {(\p s -> KW_Extends   p)}
    "if"                                {(\p s -> KW_If        p)}
    "else"                              {(\p s -> KW_Else      p)}
    "for"                               {(\p s -> KW_For       p)}
    "do"                                {(\p s -> KW_Do        p)}
    "while"                             {(\p s -> KW_While     p)}
    "print"                             {(\p s -> KW_Print     p)}
    $digit+                             {(\p s -> Int          p (read s))}
    [$alpha \_] [$alpha $digit \_]*     {(\p s -> Var          p s)}
    "++"                                {(\p s -> Sym          p s)}
    "+="                                {(\p s -> Sym          p s)}
    "--"                                {(\p s -> Sym          p s)}
    "-="                                {(\p s -> Sym          p s)}
    "=="                                {(\p s -> Sym          p s)}
    "<="                                {(\p s -> Sym          p s)}
    ">="                                {(\p s -> Sym          p s)}
    [\+\-\*\/\=\<\>\(\)\{\}\[\]\;\,\.]  {(\p s -> Sym          p s)}
    \'(@esc_char|\\\'|[^\\])\'          {(\p s -> DChar        p (unescape s))}
    \"($reg_char|\\\"|@esc_char)*\"     {(\p s -> DStr         p ((read s) :: String))}
{

-- be careful with DStr; you may get lost when dealing with escaping.
--
-- if the coder writes this in a DL program:
--      byte[] s = "Hello\n";
--
-- then the string literal will be stored as a list of 6 characters:
--      H e l l o \n
--      1 2 3 4 5  6
--
-- to unescape it back to the original form, use `show`:
--      show "Hello\n" => ['"', 'H', 'e', 'l', 'l', 'o', '\\', 'n', '"']
--                          1    2    3    4    5    6     7    8    9
-- you will get a list of 9 characters, including the double quotes.
-- it's useful when writing the string to assembly code.

data Token = KW_Return      AlexPosn
           | KW_Int         AlexPosn
           | KW_Int32       AlexPosn
           | KW_Byte        AlexPosn
           | KW_Bool        AlexPosn
           | KW_True        AlexPosn
           | KW_False       AlexPosn
           | KW_Void        AlexPosn
           | KW_Class       AlexPosn
           | KW_Public      AlexPosn
           | KW_Protected   AlexPosn
           | KW_Self        AlexPosn
           | KW_Super       AlexPosn
           | KW_New         AlexPosn
           | KW_Extends     AlexPosn
           | KW_If          AlexPosn
           | KW_Else        AlexPosn
           | KW_For         AlexPosn
           | KW_Do          AlexPosn
           | KW_While       AlexPosn
           | KW_Print       AlexPosn
           | Int            AlexPosn Int
           | Var            AlexPosn String
           | Sym            AlexPosn String -- ";", "(", "{", "++", "-", ...
           | DChar          AlexPosn Int -- '\n', '\t', '0'... (in ascii code)
           | DStr           AlexPosn String -- "\"Hello\\n\"" <= "Hello\n"
             deriving (Eq, Show)

-- just for fun... I miss Alan so much... :3
unescape :: String -> Int
unescape "\'\"\'" = ord '"'
unescape x = (ord . head . (read :: String -> String ) . ("\"" ++) . (++ "\"") . tail . init) x

token_print :: [Token] -> IO ()
token_print = tp 0
    where
        indent :: Int -> IO ()
        indent n = putStr $ concat $ take n $ repeat "    "

        tp :: Int -> [Token] -> IO ()
        tp _ [] = return ()
        tp n (t@(Sym p "{"):ts) = do {indent n;     putStrLn $ show t; tp (n+1) ts}
        tp n (t@(Sym p "}"):ts) = do {indent (n-1); putStrLn $ show t; tp (n-1) ts}
        tp n (t:ts)             = do {indent n;     putStrLn $ show t; tp n     ts}

token_posn (KW_Return     p)  = p
token_posn (KW_Int        p)  = p
token_posn (KW_Int32      p)  = p
token_posn (KW_Byte       p)  = p
token_posn (KW_Bool       p)  = p
token_posn (KW_True       p)  = p
token_posn (KW_False      p)  = p
token_posn (KW_Void       p)  = p
token_posn (KW_Class      p)  = p
token_posn (KW_Public     p)  = p
token_posn (KW_Protected  p)  = p
token_posn (KW_Self       p)  = p
token_posn (KW_Super      p)  = p
token_posn (KW_New        p)  = p
token_posn (KW_Extends    p)  = p
token_posn (KW_If         p)  = p
token_posn (KW_Else       p)  = p
token_posn (KW_For        p)  = p
token_posn (KW_Do         p)  = p
token_posn (KW_While      p)  = p
token_posn (KW_Print      p)  = p
token_posn (Int         p _)  = p
token_posn (Var         p _)  = p
token_posn (Sym         p _)  = p
token_posn (DChar       p _)  = p
token_posn (DStr        p _)  = p
}
