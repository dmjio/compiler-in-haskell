module DLC.Compile
    (compileTo)
where

import Data.Map hiding (map, foldl)
import System.IO (hPutStr, hPutStrLn, Handle)

import DLC.CompileDataObject

compileTo :: Handle -> CDO -> IO ()
compileTo h cdo =
    let r1 = foldl (compileClass cdo) ([], []) (cdoGetClasses cdo)
        (asData, asText) = foldl (compileMethods cdo) r1 (cdoGetMethods cdo)
    in do
        writeDataSection h asData
        writeTextSection h asText

writeDataSection :: Handle -> [String] -> IO ()
writeDataSection h d = do {hPutStrLn h ".data"; mapM_ (hPutStrLn h) d}

writeTextSection :: Handle -> [String] -> IO ()
writeTextSection h d = do {hPutStrLn h ".text"; mapM_ (hPutStrLn h) d}

compileClass :: CDO -> ([String], [String]) -> String -> ([String], [String])
compileClass cdo (asData, asText) cName =
    let (_, superName, attrList, methodList) = cdoGetClassDef cdo cName
        methodTable :: [String]
        methodTable = cdoGetClassMethodTable cdo cName
        methodTable' :: [String] -- className$$methodName
        methodTable' = map (\cm -> case resolveClassName cm of
                                        Nothing -> error $ "cannot find def of " ++ cm
                                        Just x -> (x ++ "$" ++ cm))
                           methodTable
        sup = if cName == "Object" then "0" else superName ++ "$$"
        asData' = asData ++ [cName ++ "$$:", "    .quad " ++ sup] ++
                  map ("    .quad " ++) methodTable'
        resolveClassName :: String -> Maybe String
        resolveClassName cmName = r cName
            where r :: String -> Maybe String
                  r cName =
                    case cdoGetClassDef cdo cName of
                        (_, _, _, cmDefList) ->
                            if any (\(_, _, (m, _, _, _)) -> m == cmName) cmDefList
                            then Just cName
                            else if cName == "Object"
                                 then Nothing
                                 else r $ cdoGetSuperClass cdo cName
    in
        (asData', asText)

compileMethods :: CDO -> ([String], [String]) -> String -> ([String], [String])
compileMethods cdo (asData, asText) mName = (asData, asText) -- FIXME
