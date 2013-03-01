module DLC.Compile
    (compileTo)
where

import Data.Map hiding (map, foldl)
import System.IO (hPutStr, hPutStrLn, Handle)
import Text.Regex.Posix

import DLC.CompileDataObject
import DLC.TAST

regTable :: [[String]]
regTable = [
    ["%rax", "%eax",  "%ax",  "%al"],
    ["%rdi", "%edi",  "%di", "%dil"],
    ["%rsi", "%esi",  "%si", "%sil"],
    ["%rdx", "%edx",  "%dx",  "%dl"],
    ["%rcx", "%ecx",  "%cx",  "%cl"],
    [ "%r8", "%r8d", "%r8w", "%r8b"],
    [ "%r9", "%r9d", "%r9w", "%r9b"]]

compileTo :: Handle -> CDO -> IO ()
compileTo h cdo =
    let r1 = foldl (compileClass cdo) ([], []) (cdoGetClasses cdo)
        (asData, asText) = foldl (compileMethod cdo Nothing) r1 (cdoGetMethods cdo)
    in do
        writeDataSection h asData
        writeTextSection h asText

writeDataSection :: Handle -> [String] -> IO ()
writeDataSection h d = do {hPutStrLn h ".data"; mapM_ (hPutStrLn h) d}

writeTextSection :: Handle -> [String] -> IO ()
writeTextSection h d = do
    hPutStrLn h ".text"
    mapM_ (\x -> if (x =~ "^.*:$") :: Bool
                 then hPutStrLn h x
                 else hPutStrLn h $ "    " ++ x) d

compileClass :: CDO -> ([String], [String]) -> String -> ([String], [String])
compileClass cdo (asData, asText) cName =
    let (_, superName, attrList, methodList) = cdoGetClassDef cdo cName
        -- generating method table; changes:
        --     asData => asData'
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
        foldl (\r (_, _, (m, _, _, _)) -> compileMethod cdo (Just cName) r m)
              (asData', asText) methodList

-- when compileMethod is applied to class attribute methods,
-- the 2nd method is "Just superClassName".
-- else it should be Nothing.
compileMethod :: CDO -> Maybe String -> ([String], [String]) -> String -> ([String], [String])
compileMethod cdo cName (asData, asText) mName =
    let mName' = case cName of
                    (Just x) -> x ++ "$" ++ mName
                    Nothing -> mName
        cName' = case cName of
                    (Just x) -> x
                    Nothing -> ""
        mDef :: TMethodDef -- (String, TType, [(String, TType)], [TBodyStmt])
        (isStatic, mDef) = if cName /= Nothing
                           then case cdoGetClassMethodDef cdo cName' mName of
                                    (Just (_, y, x)) -> (y, x)
                                    Nothing -> error $ "no def of " ++ mName ++ " in " ++ cName'
                           else (False, cdoGetMethodDef cdo mName)
        (tp, argList, mBody) =
            case mDef of
                (_, mt, mArgs, mBody) ->
                    (mt, if cName /= Nothing && not isStatic
                         then (mName, TClass cName'):mArgs
                         else mArgs,
                     mBody)
        -- now we have:
        -- cName:   Maybe String       => class name
        -- mName:   String             => method name
        -- tp:      TType              => return type
        -- argList: [(String, TType)]  => argument list (modified for class attr methods)
        -- mBody:   [TBodyStmt]        => method body
        --
        -- mName':  String  => className$methodName or just methodName
        stackUsage :: [(String, TType)]
        stackUsage = argList -- yes they should be the same
        beforeBody :: [String]
        beforeBody = ["push %rbp", "mov %rsp, %rbp"] ++
                     map (\(rIdx, _) -> "push " ++ (regTable !! rIdx !! 0))
                         (zip [1..6] argList) ++
                     (concat $ map (\(offset, _) ->
                                        ["mov " ++ (show offset) ++ "(%rbp), %rax",
                                         "push %rax"])
                                   $ reverse (zip [16, 24..] $ reverse $ drop 6 argList))
        afterBody :: [String]
        afterBody = ["add $" ++ (show $ 8 * (length stackUsage)) ++ ", %rsp",
                     "pop %rbp",
                     "ret"]
        asDataInc :: [String]
        asMethodBody :: [String]
        (asDataInc, asMethodBody, _, _) = foldl cBodyStmt ([], [], 1, stackUsage) mBody
    in
        (asData ++ asDataInc,
         asText ++ [mName' ++ ":"] ++ beforeBody ++ asMethodBody ++ afterBody)

--             .data      .text  jmpTag  stack usage
cBodyStmt :: ([String], [String], Int, [(String, TType)]) -> TBodyStmt ->
             ([String], [String], Int, [(String, TType)])
cBodyStmt x _ = x -- FIXME
