{-# LANGUAGE TypeSynonymInstances #-}

module Database.Gruze.QueryDef (

-- classes
GrzQueryTypeClass(hasIn,hasBetween,hasOp),

-- booleans
hasTrue, hasFalse,

-- filter by specific objects
withObj, withObjs,

-- by type 
hasType, hasTypes,

-- by enabled/disabled
hasEnabled, hasDisabled,

-- by the fixed relationships
hasOwner, hasContainer, hasSite,
hasOwners, hasContainers, hasSites,

-- by general relationships
hasRel,

-- by searchable fields
hasSearchable,

-- has the specified names defined
hasData,

-- return objects with the given metadata in the results
withData,

-- internal functions needed by the IO module
setQueryType, setOrderBy, setAggOrderBy

) where

import Database.Gruze.Types
import Database.Gruze.Box

import Data.List (intercalate, foldl')
import Data.Maybe
import Data.Typeable

grzMakeQueryDefName :: String -> GrzQDWFItem
grzMakeQueryDefName s = GrzQDName s

-- some private query functions

setQueryType :: GrzQueryType -> GrzQueryDef -> GrzQueryDef
setQueryType t ((m,n),x) =
    ((m,n), (GrzQDType t) : x)
    
setOrderBy :: [GrzOrderBy] -> GrzQueryDef -> GrzQueryDef
setOrderBy (ob:obs) = (setOrderBy obs) . (setOrderByItem ob False)
setOrderBy [] = id

setAggOrderBy :: [GrzOrderBy] -> GrzQueryDef -> GrzQueryDef
setAggOrderBy (ob:obs) = (setAggOrderBy obs) . (setOrderByItem ob True)
setAggOrderBy [] = id

setOrderByItem ob isAgg =
    case ob of
        StringAsc s -> setOrderByMetadataItem ob s isAgg
        StringDesc s -> setOrderByMetadataItem ob s isAgg
        IntAsc s -> setOrderByMetadataItem ob s isAgg
        IntDesc s -> setOrderByMetadataItem ob s isAgg
        otherwise -> setOrderByOrdinaryItem ob
        
setOrderByOrdinaryItem ob ((m,n),x) =
    ((m,n),(GrzQDOrderBy (-1) (getOrderBit ob n)) :x)       

setOrderByMetadataItem :: GrzOrderBy -> String -> Bool -> GrzQueryDef -> GrzQueryDef
setOrderByMetadataItem ob s isAgg ((m,n),x) =
    ((m,n+1),(GrzQDOrderBy realM (getOrderBit ob n))
    : (GrzQDSelect (getSelectBit ob n))
    : (GrzQDGroupBy realM ("mob" ++ (show n) ++ ".objectGuid"))
    : (GrzQDJoin realM ("metadata mob" ++ (show n) ++ " ON (mob" ++ (show n) ++ ".objectGuid = obj" ++ (show realM) ++ ".guid)")) 
    : (GrzQDWhereFrags realM
            (
                [GrzQDString 
                    ("mob" ++ (show n) ++  ".nameId = "
                    )
                 ] 
                ++ [GrzQDName s] 
            )
        )
      : x)
    where    
        realM = if isAgg then m-1 else m
        
getSelectBit :: GrzOrderBy -> Int -> String
getSelectBit ob n =
    case ob of
        StringAsc _ -> "max(mob" ++ (show n) ++ ".stringValue) AS mob" ++ (show n) ++ "_stringValue"
        StringDesc _ -> "max(mob" ++ (show n) ++ ".stringValue) AS mob" ++ (show n) ++ "_stringValue"
        IntAsc _ -> "max(mob" ++ (show n) ++ ".integerValue) AS mob" ++ (show n) ++ "_integerValue"
        IntDesc _ -> "max(mob" ++ (show n) ++ ".integerValue) AS mob" ++ (show n) ++ "_integerValue"

getOrderBit :: GrzOrderBy -> Int -> String
getOrderBit ob n =
    case ob of
        GuidAsc -> "objGuid ASC"
        GuidDesc -> "objGuid DESC"
        TimeCreatedAsc -> "timeCreated ASC"
        TimeCreatedDesc -> "timeCreated DESC"
        TimeUpdatedAsc -> "timeUpdated ASC"
        TimeUpdatedDesc -> "timeUpdated DESC"
        StringAsc _ -> "q1.mob" ++ (show n) ++ "_stringValue ASC"
        StringDesc _ -> "q1.mob" ++ (show n) ++ "_stringValue DESC"
        IntAsc _ -> "q1.mob" ++ (show n) ++ "_integerValue ASC"
        IntDesc _ -> "q1.mob" ++ (show n) ++ "_integerValue DESC"
        CountAsc -> "grzCount ASC"
        CountDesc -> "grzCount DESC"
        SumAsc -> "grzSum ASC"
        SumDesc -> "grzSum DESC"
        -- TODO: AvgAsc and AvgDesc

-- the public query functions

withObjs :: GrzObjClass o => [o] -> GrzQueryDef -> GrzQueryDef  
withObjs objList ((m,n), x) =
   ((m,n), (GrzQDWhere m ("obj" ++ (show m) ++ ".guid IN (" ++ (objListToString objList) ++ ")")) : x)
   
withObj o = withObjs [o]

hasEnabled :: GrzQueryDef -> GrzQueryDef
hasEnabled ((m,n), x) =
    ((m,n), (GrzQDWhere m ("obj" ++ (show m) ++ ".enabled = 1")) : x)

hasDisabled :: GrzQueryDef -> GrzQueryDef 
hasDisabled ((m,n), x) =
    ((m,n), (GrzQDWhere m ("obj" ++ (show m) ++ ".enabled = 0")) : x)

hasTypes :: (Typeable o, GrzObjClass o) => [GrzObj -> o] -> GrzQueryDef -> GrzQueryDef  
hasTypes tcList ((m,n), x) =
   ((m,n), (GrzQDWhereFrags m ([GrzQDString ("obj" ++ (show m) ++ ".objectType IN (")] 
        ++ [GrzQDNameList (map (\y -> objWrapperToString $ (y emptyObj)) tcList)] 
        ++ [GrzQDString ")"]))  : x)
        
hasType ot = hasTypes [ot]

refDict :: [(GrzRef,String)]
refDict = [(ObjRef,"guid"),(ContainerRef,"containerGuid"), (OwnerRef,"ownerGuid"),(SiteRef,"siteGuid")]

-- a non-public utility function to avoid writing the same code many times
hasFixed :: GrzObjClass o => GrzRef -> [o] -> GrzQueryDef -> GrzQueryDef
hasFixed ref objList ((m,n), x) =
    ((m,n), (GrzQDWhere m ("obj" ++ (show m) ++ "." ++ field ++ " IN (" ++ (objListToString objList) ++ ")")) : x)
    where
        field = fromJust $ lookup ref refDict

-- the public functions
hasOwners :: GrzObjClass o => [o] -> GrzQueryDef -> GrzQueryDef   
hasOwners = hasFixed OwnerRef
    
hasOwner o = hasOwners [o]

hasContainers :: GrzObjClass o => [o] -> GrzQueryDef -> GrzQueryDef
hasContainers = hasFixed ContainerRef
    
hasContainer o = hasContainers [o]

hasSites :: GrzObjClass o => [o] -> GrzQueryDef -> GrzQueryDef
hasSites = hasFixed SiteRef
   
hasSite o = hasSites [o]

-- relationships

hasRel :: String 
    -> GrzQueryDef
    -> GrzQueryDef
hasRel rel =
    if isSpecial rel'
        then handleRelSpecial rel' dir
        else handleRel rel' dir
    where
        dir = if null rel
                then ForwardRel
                else if head rel == '-'
                        then BackwardRel
                        else ForwardRel
        rel' = if null rel
                then rel
                else if (head rel) `elem` ['-','+']
                        then tail rel
                        else rel
        isSpecial rel = rel `elem` ["hasContainer","hasOwner","hasSite"]

-- the three special relationships: hasContainer, hasOwner and hasSite
-- can be implemented without the relationship table and so
-- can avoid a join
handleRelSpecial :: String
    -> GrzRelDir    
    -> GrzQueryDef
    -> GrzQueryDef
handleRelSpecial rel dir ((m,n), x) =
    ((m+1,n),
        (GrzQDJoin (m+1) ("objects obj" ++ (show (m+1)) ++ " ON (obj"
            ++ (show m) ++ guidA ++ " = obj" ++ (show (m+1)) ++ guidB ++ ")"))
         : x)
    where
        field = case rel of
                    "hasContainer" -> ".containerGuid"
                    "hasOwner" -> ".ownerGuid"
                    "hasSite" -> ".siteGuid"
        guidA = case dir of 
                    ForwardRel -> field
                    BackwardRel -> ".guid"
        guidB = case dir of
                    ForwardRel -> ".guid"
                    BackwardRel -> field

-- the usual case with a join to the relationship table   
handleRel :: String
    -> GrzRelDir    
    -> GrzQueryDef
    -> GrzQueryDef
handleRel rel dir ((m,n), x) =
    ((m+1,n),
        (GrzQDJoin (m+1) ("objects obj" ++ (show (m+1)) ++ " ON (r"
            ++ (show m) ++ guidB ++ " = obj" ++ (show (m+1)) ++ ".guid)"))
        : (GrzQDJoin m ("relationships r" ++ (show m) ++ " ON (r"
            ++ (show m) ++ guidA ++ " = obj" ++ (show m) ++ ".guid)"))
        : (GrzQDWhereFrags m
            (
                [GrzQDString 
                    ("r" ++ (show m) ++  ".relationshipType = "
                    )
                 ] 
                ++ [GrzQDName rel] 
            )
        ) : x)
     where
        guidA = case dir of 
                    ForwardRel -> ".guid1"
                    BackwardRel -> ".guid2"
        guidB = case dir of
                    ForwardRel -> ".guid2"
                    BackwardRel -> ".guid1"
       
hasAtomOp :: String -> String -> [GrzAtom] -> GrzQueryDef -> GrzQueryDef   
hasAtomOp name op values ((m,n), x) =
    ((m,n+1),(GrzQDJoin m ("metadata m" ++ mbit 
        ++ " ON (obj" ++ (show m) ++ ".guid = m" ++ mbit ++ ".objectGuid)")) 
    : (GrzQDWhereFrags m ([GrzQDString ("m" ++ mbit ++ ".nameId = ")] 
        ++ ([GrzQDName name]) 
        ++ [GrzQDString (" AND ")]
        ++ [getAtomClause values (m,n) op]))
    : x)
    where
        mbit = (show m) ++ "_" ++ (show n)
    
hasData :: String -> GrzQueryDef -> GrzQueryDef   
hasData name ((m,n), x) =
    ((m,n+1),(GrzQDJoin m ("metadata m" ++ mbit 
        ++ " ON (obj" ++ (show m) ++ ".guid = m" ++ mbit ++ ".objectGuid)")) 
    : (GrzQDWhereFrags m ([GrzQDString ("m" ++ mbit ++ ".nameId = ")] 
        ++ [GrzQDName name] 
        ))
    : x)
    where
        mbit = (show m) ++ "_" ++ (show n)
    
hasSearchable :: String -> GrzQueryDef -> GrzQueryDef   
hasSearchable content ((m,n), x) =
    ((m,n+1),(GrzQDJoin m ("metadata m" ++ (show m) ++ "_" ++ (show n) 
        ++ " ON (m" ++ (show m) ++ "_" ++ (show n) ++ ".nameID = s" ++ (show n) ++ ".nameID)"))
    : (GrzQDJoin m ("searchable s" ++ (show n) 
        ++ " ON (obj" ++ (show m) ++ ".objectType = s" ++ (show n) ++ ".typeID)"))
    : (GrzQDWhereFrags m ([getAtomClause [stringToAtom content] (m,n) "match"]))
    : (GrzQDWhere m ("m" ++ (show m) ++ "_" ++ (show n) ++ ".objectGuid = obj" ++ (show m) ++ ".guid"))
    : x)

hasAtomIn :: String -> [GrzAtom] -> GrzQueryDef -> GrzQueryDef   
hasAtomIn name values = hasAtomOp name "IN" values

class GrzQueryTypeClass qt where
    hasIn :: GrzKey -> [qt] -> GrzQueryDef -> GrzQueryDef
    hasBetween :: GrzKey -> (qt,qt) -> GrzQueryDef -> GrzQueryDef
    hasOp :: GrzKey -> String -> qt -> GrzQueryDef -> GrzQueryDef
    
instance GrzQueryTypeClass GrzString where
    hasIn name values = hasAtomIn name (map stringToAtom values)
    hasBetween name (v0,v1) = hasAtomOp name "><" (map stringToAtom [v0,v1])
    hasOp name op value = hasAtomOp name op (map stringToAtom [value])
    
instance GrzQueryTypeClass GrzInt where
    hasIn name values = hasAtomIn name (map intToAtom values)
    hasBetween name (v0,v1) = hasAtomOp name "><" (map intToAtom [v0,v1]) 
    hasOp name op value = hasAtomOp name op (map intToAtom [value])
    
hasTrue :: String -> GrzQueryDef -> GrzQueryDef
hasTrue name = hasAtomIn name (map boolToAtom [True])

hasFalse :: String -> GrzQueryDef -> GrzQueryDef
hasFalse name = hasAtomIn name (map boolToAtom [False])

withData :: [String] -> GrzQueryDef -> GrzQueryDef
withData vs ((m,n), x) = foldl' (\((m,n),x) v -> ((m,n+1),(GrzQDNeeds v n):x)) ((m,n),x) vs

-- various simple utility functions

objListToString :: GrzObjClass o => [o] -> String
-- a bit of a kludge
objListToString [] = "-1"
objListToString x =
    intercalate ", " (map (show . getID . unwrapObj) x)

grzSafeAtomListToIntString :: [GrzAtom] -> String
grzSafeAtomListToIntString x =
    intercalate ", " (map (show . safeAtomToInt) x)

-- given a list of atoms, a join number and an operation, generates appropriate where value clauses

getAtomClause :: [GrzAtom] -> (Int,Int) -> String -> GrzQDWFItem   
getAtomClause atoms (m,n) op = 
        GrzQDAtomClause valid intClause boolClause stringClause ints bools strings
    where
        mname = "m" ++ (show m) ++ "_" ++ (show n)
        intAtoms = filter isIntAtom atoms
        intMarks = intercalate "," $ map toMark intAtoms
        ints = map atomToInt intAtoms
        boolAtoms = filter isBoolAtom atoms
        boolMarks = intercalate "," $ map toMark boolAtoms
        bools = map safeAtomToInt boolAtoms
        stringAtoms = filter isStringAtom atoms
        stringMarks = intercalate "," $ map toMark stringAtoms
        strings = if (op == "match") -- add % on either side
                    then map (\x -> ("%" ++ (atomToString x) ++ "%")) stringAtoms
                    else map atomToString stringAtoms
        maybeOp = lookup op normaliseOp

        valid = ((isJust maybeOp)
                    && case maybeOp of
                        Just "><" -> ((length atoms) == 2) 
                                        && (((length intAtoms) == 2) 
                                              || ((length boolAtoms) == 2) 
                                              || ((length stringAtoms) == 2))
                        otherwise -> True
                )
        vop = if valid then fromJust maybeOp else ""

        intClause = if null ints 
                        then "" 
                        else "(" ++ mname ++ ".metadataType = 0 AND " 
                            ++ (getOpBit op (mname ++ ".integerValue") intAtoms) ++ ")"
        boolClause = if null bools 
                        then "" 
                        else "(" ++ mname ++ ".metadataType = 1 AND " 
                            ++ (getOpBit op (mname ++ ".integerValue") boolAtoms)  ++ ")"
        stringClause = if null strings 
                        then "" 
                        else "(" ++ mname ++ ".metadataType = 2 AND " 
                            ++ (getOpBit op (mname ++ ".stringValue") stringAtoms)  ++ ")"

-- normalises op
normaliseOp = [("in", "IN"),("In","IN"),("IN","IN"),("iN","IN"),("><","><") ,
    ("<","<"),("<=","<="),("=","="),(">",">"),(">=",">="),("match","match")]
                        
-- generate the condition bit
-- TODO: handle other conditions?
-- eg. inclusive between        
getOpBit :: String -> String -> [GrzAtom] -> String
getOpBit "IN" var atoms = " ( " ++ var ++ " IN (" ++ (intercalate "," $ map toMark atoms) ++ ")) "
getOpBit "><" var _ = " ( " ++ var ++ " > ? AND " ++ var ++ " < ? ) "
getOpBit "match" var _ = " ( " ++ var ++ " LIKE ? ) "
getOpBit op var _ = " ( " ++ var ++ " " ++ op ++ " ? ) "

toMark _ = "?"                        
        
grzAtomListToStrings :: [GrzAtom] -> String -> String -> [String]
grzAtomListToStrings values start end =
    reverse $ (grzAtomListToStrings2 values [start] end)
    where
        grzAtomListToStrings2 :: [GrzAtom] -> [String] -> String -> [String]
        grzAtomListToStrings2 ((GrzAtomString s) : vs) xs end = 
            grzAtomListToStrings2 vs("," : s : xs) end
        grzAtomListToStrings2 (other : vs) (x:xs) end = 
            grzAtomListToStrings2 vs ((x ++ (show $ safeAtomToInt other) ++ ",") : xs) end
        grzAtomListToStrings2 (other : vs) [] end = 
            grzAtomListToStrings2 vs [(show $ safeAtomToInt other) ++ ","] end
        grzAtomListToStrings2 [] (x:xs) end = ((grzTrimComma x ++ end): xs)
        grzAtomListToStrings2 [] [] _ = []
        
grzMakeQueryStrings :: [String] -> String -> String -> [String]
grzMakeQueryStrings values start end =
    reverse $ (grzMakeQueryStrings2 values [start] end)
    where
        grzMakeQueryStrings2 :: [String] -> [String] -> String -> [String]
        grzMakeQueryStrings2 (s : vs) xs end = grzMakeQueryStrings2 vs ("," : s : xs) end
        grzMakeQueryStrings2 [] (x:xs) end = ((grzTrimComma x ++ end): xs)
        grzMakeQueryStrings2 [] [] _ = []

grzTrimComma x =
    if (last x == ',') then init x else x