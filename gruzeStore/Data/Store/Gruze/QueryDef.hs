{-# LANGUAGE TypeSynonymInstances #-}

module Data.Store.Gruze.QueryDef (

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

-- an internal function needed by the IO module
-- TODO: hide this
setQueryType

) where

import Data.Store.Gruze.Types
import Data.Store.Gruze.Box

import Data.List (intercalate, foldl')
import Data.Maybe
import Data.Typeable

-- TODO: add order functions
-- need order by sum, count and avg as well

grzMakeQueryDefName :: String -> GrzQDWFItem
grzMakeQueryDefName s = GrzQDName s

setQueryType :: GrzQueryType -> GrzQueryDef -> GrzQueryDef
setQueryType t ((m,n),x) =
    ((m,n), (GrzQDType t) : x)

withObjs :: GrzObjClass o => [o] -> GrzQueryDef -> GrzQueryDef  
withObjs objList ((m,n), x) =
   ((m,n), (GrzQDWhere ("obj" ++ (show m) ++ ".guid IN (" ++ (objListToString objList) ++ ")")) : x)
   
withObj o = withObjs [o]

hasEnabled :: GrzQueryDef -> GrzQueryDef
hasEnabled ((m,n), x) =
    ((m,n), (GrzQDWhere ("obj" ++ (show m) ++ ".enabled = 1")) : x)

hasDisabled :: GrzQueryDef -> GrzQueryDef 
hasDisabled ((m,n), x) =
    ((m,n), (GrzQDWhere ("obj" ++ (show m) ++ ".enabled = 0")) : x)

    -- TODO inner aggregation affects the type of the result so this bit should be added
    -- to the *outer* query I think?
    -- perhaps have a GrzQDOuterWhereFrags ?
    -- or for efficiency, add another inner join inside and specify the type
    -- (eg. for the container or the owner) there?
    -- in any case, inner aggregation needs a way to specify two types - the
    -- type of the object being aggregated and the type of the object returned
hasTypes :: (Typeable o, GrzObjClass o) => [GrzObj -> o] -> GrzQueryDef -> GrzQueryDef  
hasTypes tcList ((m,n), x) =
   ((m,n), (GrzQDWhereFrags ([GrzQDString ("obj" ++ (show m) ++ ".objectType IN (")] 
        ++ [GrzQDNameList (map (\y -> objWrapperToString $ (y emptyObj)) tcList)] 
        ++ [GrzQDString ")"]))  : x)
        
hasType ot = hasTypes [ot]

refDict :: [(GrzRef,String)]
refDict = [(ObjRef,"guid"),(ContainerRef,"containerGuid"), (OwnerRef,"ownerGuid"),(SiteRef,"siteGuid")]

-- a non-public utility function to avoid writing the same code many times
hasFixed :: GrzObjClass o => GrzRef -> [o] -> GrzQueryDef -> GrzQueryDef
hasFixed ref objList ((m,n), x) =
    ((m,n), (GrzQDWhere ("obj" ++ (show m) ++ "." ++ field ++ " IN (" ++ (objListToString objList) ++ ")")) : x)
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

hasRel :: String 
    -> GrzRelDir 
    -> (GrzQueryDef -> GrzQueryDef)
    -> GrzQueryDef
    -> GrzQueryDef
hasRel rel dir qd ((m,n), x) =
    qd (handleOuterRel rel dir ((m,n), x))
       
handleOuterRel :: String
    -> GrzRelDir    
    -> GrzQueryDef
    -> GrzQueryDef
handleOuterRel rel dir ((m,n), x) =
    ((m+1,n),
        (GrzQDJoin ("INNER JOIN objects obj" ++ (show (m+1)) ++ " ON (r"
            ++ (show m) ++ guidB ++ " = obj" ++ (show (m+1)) ++ ".guid)"))
        : (GrzQDJoin ("INNER JOIN relationships r" ++ (show m) ++ " ON (r"
            ++ (show m) ++ guidA ++ " = obj" ++ (show m) ++ ".guid)"))
        : (GrzQDWhereFrags 
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
    
-- hasRel :: GrzObjClass o => 
--     (String, GrzRelDir) 
--     -> GrzRef 
--     -> [o] 
--     -> GrzQueryDef 
--     -> GrzQueryDef
-- hasRel (rel,dir) ref objList ((m,n), x) =
--     ((m,n+1),
--         (GrzQDJoin ("INNER JOIN relationships r" ++ (show m) ++ "_" ++ (show n) ++ " ON ("
--             ++ guidA ++ " = obj" ++ (show m) ++ "." ++ field ++ ")"))
--         : (GrzQDWhereFrags 
--             (
--                 [GrzQDString 
--                     (guidB ++ " IN (" ++ (objListToString objList) ++ ") "
--                         ++ "AND r" ++ (show m) ++ "_" ++ (show n) ++ ".relationshipType IN ("
--                     )
--                  ] 
--                 ++ [GrzQDNameList [rel]] 
--                 ++ [GrzQDString ")"]
--             )
--         ) : x)
--      where
--         field = fromJust $ lookup ref refDict
--         guidA = case dir of 
--                     FwdRelDir -> "guid1"
--                     otherwise -> "guid2"
--         guidB = case dir of
--                     FwdRelDir -> "guid2"
--                     otherwise -> "guid1"
--                       
-- hasRel2 :: GrzObjClass o => 
--     (String, GrzRelDir) 
--     -> (String, GrzRelDir) 
--     -> GrzRef
--     -> String
--     -> [o] 
--     -> GrzQueryDef
--     -> GrzQueryDef
-- hasRel2 (rel1,dir1) (rel2,dir2) ref agg objList ((m,n), x) =
--     ((m,n+1), 
--         (GrzQDJoin ("INNER JOIN relationships " ++ r2 ++ " ON (obj" ++ (show m) ++ "." ++ field 
--             ++ " = " ++ r2 ++ "." ++ guidBr2 ++ ")"))
--         : (GrzQDJoin ("INNER JOIN relationships " ++ r1 ++ " ON (" ++ r1 ++ "." 
--             ++ guidBr1 ++ " = " ++ r2 ++ "." ++ guidAr2 ++ ")"))
--         : (GrzQDAgg agg)
--         : (GrzQDWhereFrags 
--             (
--                 [GrzQDString 
--                     (r1 ++ "."
--                         ++ guidAr1
--                         ++ " IN (" ++ (objListToString objList) ++ ") "
--                         ++ "AND " ++ r1 ++ ".relationshipType = "
--                     )
--                 ] 
--                 ++ [GrzQDName rel1] 
--                 ++ [GrzQDString (" AND " ++ r2 ++ ".relationshipType = ")]
--                 ++ [GrzQDName rel2]
--             )
--         ) : x)
--     where
--         field = fromJust $ lookup ref refDict
--         r1 = "r1_" ++ (show n)
--         r2 = "r2_" ++ (show n)
--         guidAr1 = case dir1 of
--                     FwdRelDir -> "guid1"
--                     otherwise -> "guid2"
--         guidBr1 = case dir1 of
--                     FwdRelDir -> "guid2"
--                     otherwise -> "guid1"
--         guidAr2 = case dir2 of
--                     FwdRelDir -> "guid1"
--                     otherwise -> "guid2"
--         guidBr2 = case dir2 of
--                     FwdRelDir -> "guid2"
--                     otherwise -> "guid1"
   
hasAtomOp :: String -> String -> [GrzAtom] -> GrzQueryDef -> GrzQueryDef   
hasAtomOp name op values ((m,n), x) =
    ((m,n+1),(GrzQDJoin ("INNER JOIN metadata m" ++ mbit 
        ++ " ON (obj" ++ (show m) ++ ".guid = m" ++ mbit ++ ".objectGuid)")) 
    : (GrzQDWhereFrags ([GrzQDString ("m" ++ mbit ++ ".nameId = ")] 
        ++ ([GrzQDName name]) 
        ++ [GrzQDString (" AND ")]
        ++ [getAtomClause values (m,n) op]))
    : x)
    where
        mbit = (show m) ++ "_" ++ (show n)
    
hasData :: String -> GrzQueryDef -> GrzQueryDef   
hasData name ((m,n), x) =
    ((m,n+1),(GrzQDJoin ("INNER JOIN metadata m" ++ mbit 
        ++ " ON (obj" ++ (show m) ++ ".guid = m" ++ mbit ++ ".objectGuid)")) 
    : (GrzQDWhereFrags ([GrzQDString ("m" ++ mbit ++ ".nameId = ")] 
        ++ [GrzQDName name] 
        ))
    : x)
    where
        mbit = (show m) ++ "_" ++ (show n)
    
hasSearchable :: String -> GrzQueryDef -> GrzQueryDef   
hasSearchable content ((m,n), x) =
    ((m,n+1),(GrzQDJoin ("INNER JOIN metadata m" ++ (show m) ++ "_" ++ (show n) 
        ++ " ON (m" ++ (show m) ++ "_" ++ (show n) ++ ".nameID = s" ++ (show n) ++ ".nameID)"))
    : (GrzQDJoin ("INNER JOIN searchable s" ++ (show n) 
        ++ " ON (obj" ++ (show m) ++ ".objectType = s" ++ (show n) ++ ".typeID)"))
    : (GrzQDWhereFrags ([getAtomClause [stringToAtom content] (m,n) "match"]))
    : (GrzQDWhere ("m" ++ (show m) ++ "_" ++ (show n) ++ ".objectGuid = obj" ++ (show m) ++ ".guid"))
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