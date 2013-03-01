module DLC.CompileDataObject
    (CDO, CDFuncSignature, genCDO, prettyPrintCDO,
     cdoGetClasses, cdoGetMethods,
     cdoGetObjSize, cdoGetClassMethodTable, cdoGetAttrOffset,
     cdoGetSuperClass, cdoGetClassDef, cdoGetMethodDef, cdoGetClassMethodDef)
where

import Data.Map hiding (map, foldl, filter)
import Data.List (find)

import DLC.TAST
import DLC.Job

--                   className  objSize     method table             attr offset
--                                        pub/pro    private
type CDObjInfo = Map String     (Int,    ([String], [String]),      Map String Int)
type CDFuncSignature = (TType, [TType])

type CDO = (CDObjInfo,
            Map String TClassDef,
            Map String TMethodDef,
            Map String CDFuncSignature) -- C bridges

cdoGetClasses :: CDO -> [String]
cdoGetClasses cdo = case cdo of (_, cDefMap, _, _) -> keys cDefMap

cdoGetMethods :: CDO -> [String]
cdoGetMethods cdo = case cdo of (_, _, mDefMap, _) -> keys mDefMap

cdoGetClassMethodTable :: CDO -> String -> [String]
cdoGetClassMethodTable cdo cName =
    case cdo of
        (oi, _, _, _) -> case (oi ! cName) of
            (_, (mt1, mt2), _) -> mt1 ++ mt2
cdoGetObjSize :: CDO -> String -> Int
cdoGetObjSize cdo cName =
    case cdo of
        (oi, _, _, _) -> case (oi ! cName) of
            (s, _, _) -> s
cdoGetAttrOffset :: CDO -> String -> String -> Int
cdoGetAttrOffset cdo cName aName =
    case cdo of
        (oi, _, _, _) -> case (oi ! cName) of
            (_, _, m) -> m ! aName
cdoGetSuperClass :: CDO -> String -> String
cdoGetSuperClass cdo cName =
    case cdo of
        (_, cDefMap, _, _) -> case (cDefMap ! cName) of
            (_, sc, _, _) -> sc
cdoGetClassDef :: CDO -> String -> TClassDef
cdoGetClassDef cdo cName =
    case cdo of (_, cDefMap, _, _) -> cDefMap ! cName
cdoGetMethodDef :: CDO -> String -> TMethodDef
cdoGetMethodDef cdo mName =
    case cdo of (_, _, mDefMap, _) -> mDefMap ! mName
cdoGetClassMethodDef :: CDO -> String -> String -> Maybe TClassMethodDef
cdoGetClassMethodDef cdo cName mName =
    case cdoGetClassDef cdo cName of
        (_, _, _, cmDefList) -> find (\(_, _, (m, _, _, _)) -> m == mName) cmDefList

prettyPrintCDO :: CDO -> IO ()
prettyPrintCDO (oi, _, _, _) = do
    mapM_ (\cName -> putStrLn $ cName ++ " => " ++ (show (oi ! cName))) (keys oi)
    -- mapM_ (\fName -> putStrLn $ show (cBridges ! fName)) (keys cBridges)

cdoCBridges :: Map String CDFuncSignature
cdoCBridges = fromList [
    ("dlib_malloc",     (TInt,  [TInt])),
    ("dlib_free",       (TVoid, [TInt])),
    ("dlib_print_num",  (TVoid, [TInt])),
    ("dlib_print_char", (TVoid, [TByte])),
    ("dlib_arr_get",    (TInt,  [TInt, TInt, TInt])),
    ("dlib_arr_set",    (TVoid, [TInt, TInt, TInt, TInt])),
    ("dlib_memcpy",     (TVoid, [TInt, TInt, TInt]))]

-- topological sorting!
tSortClasses :: Map String TClassDef -> [String]
tSortClasses cDefMap = f (rDepMap ! "Object") ["Object"] where
    getBaseClass :: String -> String
    getBaseClass cName = case (cDefMap ! cName) of (_, bc, _ ,_) -> bc
    rDepMap :: Map String [String] -- {base class => [extended class]}
    rDepMap = foldl (\m ec -> if ec == "Object"
                                then m
                              else let bc = getBaseClass ec
                                   in if member bc m
                                      then insert bc ((m ! bc) ++ [ec]) m
                                      else insert bc [ec] m)
                    empty (keys cDefMap)
    --    queue       record
    f :: [String] -> [String] -> [String]
    f [] r = r
    f (c:cs) r = f (cs ++ if member c rDepMap
                          then rDepMap ! c
                          else [])
                   (r ++ [c])

getTypeSize :: TType -> Int
getTypeSize (TClass _)   = 8
getTypeSize (TArray _ _) = 8
getTypeSize t | t == TInt   = 8
              | t == TInt32 = 4
              | t == TByte  = 1
              | t == TBool  = 1

genCDO :: TransResult -> Job CDO
genCDO (cDefMap, m) = return (foldl f empty (tSortClasses cDefMap),
                                    cDefMap, m, cdoCBridges)
    where
    f :: CDObjInfo -> String -> CDObjInfo
    f oi cName =
        let bc = case (cDefMap ! cName) of (_, bcName, _, _) -> bcName
            bcSize = if cName == "Object"
                     then 0
                     else case (oi ! bc) of (s, _, _) -> s
            -- always hide attributes with the same names in super class
            -- 
            -- attrOffsetMap only contains attributes defined in the current class;
            -- inherited attributes should be accessed from map for the super classes.
            (_, _, attrs, _) = cDefMap ! cName
            attrList :: [(String, Int)] -- [(var name, var size)]
            attrList = map (\(_, _, (vName, tp, _)) -> (vName, getTypeSize tp)) attrs
            attrOffsetMap :: Map String Int
            (objSize, attrOffsetMap) =
                foldl (\(bs, m) (v, s) -> let offset = if bs `mod` s == 0
                                                       then bs
                                                       else bs - (bs `mod` s) + s
                                          in (offset+s, insert v offset m))
                      (bcSize, empty)
                      attrList
            -- only Public/Protected methods will be inherited&overridden.
            -- also, I'm doing overriding simply by function name...
            (bcPPMT, _) = if cName == "Object"
                          then ([], [])
                          else case (oi ! bc) of (_, mt, _) -> mt
            mList :: [(String, Bool)] -- (funcName, isPrivate?)
            mList = map (\(acc, _, (fName, _, _, _)) -> (fName, acc == TPrivate))
                        $ filter (\(_, isStatic, _) -> not isStatic)
                                 (case cDefMap ! cName of (_, _, _, t) -> t)
            methodTable :: ([String], [String])
            methodTable = foldl (\(ppmt, privatemt) (fName, isPrivate) ->
                                    if isPrivate
                                    then (ppmt, privatemt ++ [fName])
                                    else (if any (fName ==) ppmt
                                            then ppmt ++ []
                                            else ppmt ++ [fName],
                                          privatemt))
                                (bcPPMT, []) mList
        in
            insert cName (objSize, methodTable, attrOffsetMap) oi
