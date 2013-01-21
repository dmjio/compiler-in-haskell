module Compile(compileFuncTo)
    where

import System.IO
import Parser.DLCParser

-- type DataSize = Int -- in bytes
data DataSpace = Register   Int -- DataSize
               | StackSpace Int -- DataSize
                 deriving (Eq, Show)

data AsmInstr = AsmTag String
              | AsmCmd String String String
                deriving (Eq, Show)

-- FIXME: number of params is restricted to no more than 6 for now
compileFuncTo :: Handle -> Func -> IO ()
compileFuncTo f (retType, funcName, paramList, funcBody) =
    do
        hPutStr f (".globl " ++ funcName ++ "\n")
        hPutStr f (funcName ++ ":\n")
