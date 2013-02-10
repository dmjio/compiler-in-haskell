module Assembly
    where

type AssemFunc = (String, [AssemCode])
data AssemCode = AssemTag Int
               | AssemInstr String [String]
               | AssemJmp Int
                 deriving Show

stringifyAssemFunc :: AssemFunc -> [String]
stringifyAssemFunc (funcName, instrList) = [funcName ++ ":"] ++ (map showInstr instrList)
    where
        join :: String -> [String] -> String
        join sp [] = ""
        join sp (x:xs) = foldl (\a b -> a ++ sp ++ b) x xs

        indent = "    "
        conv_tag n = "DL_JTAG_" ++ funcName ++ "_" ++ (show n)

        -- WARNING: op should have the correct suffix (if applies).
        showInstr (AssemTag n) = (conv_tag n) ++ ":"
        showInstr (AssemJmp n) = indent ++ "jmp " ++ (conv_tag n)
        showInstr (AssemInstr op []) = indent ++ op
        showInstr (AssemInstr op args) = indent ++ op ++ " " ++ (join ", " args)

data Reg = P0 Int
         | P1 Int
         | P2 Int
         | P3 Int
         | P4 Int
         | P5 Int
         | P6 Int
         | SP
         | BP
           deriving Eq

instance Show Reg where
    show (P0 n) | n == 8  = "%rax"
                | n == 4  = "%eax"
                | n == 2  =  "%ax"
                | n == 1  =  "%al"

    show (P1 n) | n == 8  = "%rdi"
                | n == 4  = "%edi"
                | n == 2  =  "%di"
                | n == 1  = "%dil"

    show (P2 n) | n == 8  = "%rsi"
                | n == 4  = "%esi"
                | n == 2  =  "%si"
                | n == 1  = "%sil"

    show (P3 n) | n == 8  = "%rdx"
                | n == 4  = "%edx"
                | n == 2  =  "%dx"
                | n == 1  =  "%dl"

    show (P4 n) | n == 8  = "%rcx"
                | n == 4  = "%ecx"
                | n == 2  =  "%cx"
                | n == 1  =  "%cl"

    show (P5 n) | n == 8  =  "%r8"
                | n == 4  = "%r8d"
                | n == 2  = "%r8w"
                | n == 1  = "%r8b"

    show (P6 n) | n == 8  =  "%r9"
                | n == 4  = "%r9d"
                | n == 2  = "%r9w"
                | n == 1  = "%r9b"

    show SP = "%rsp"
    show BP = "%rbp"
