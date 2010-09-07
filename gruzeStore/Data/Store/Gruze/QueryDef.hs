module Data.Store.Gruze.QueryDef where

import Data.Store.Gruze.Types
import Data.Store.Gruze.Container

import Data.List (intercalate, foldl')
import Data.Maybe

-- TODO: add order functions

grzMakeQueryDefName :: String -> GrzQDWFItem
grzMakeQueryDefName s = GrzQDName s

setQueryType :: GrzQueryType -> GrzQueryDef -> GrzQueryDef
setQueryType t (n,x) =
    (n, (GrzQDType t) : x)

withObjs :: [GrzObj] -> GrzQueryDef -> GrzQueryDef  
withObjs objList (n, x) =
   (n, (GrzQDWhere ("obj.guid IN (" ++ (grzObjListToString objList) ++ ")")) : x)
   
withObj o = withObjs [o]

hasOwners :: [GrzObj] -> GrzQueryDef -> GrzQueryDef   
hasOwners objList (n, x) =
    (n, (GrzQDWhere ("obj.ownerGuid IN (" ++ (grzObjListToString objList) ++ ")")) : x)
    
hasOwner o = hasOwners [o]

hasContainers :: [GrzObj] -> GrzQueryDef -> GrzQueryDef
hasContainers objList (n, x) =
    (n, (GrzQDWhere ("obj.containerGuid IN (" ++ (grzObjListToString objList) ++ ")")) : x)
    
hasContainer o = hasContainers [o]

hasSites :: [GrzObj] -> GrzQueryDef -> GrzQueryDef
hasSites objList (n, x) =
   (n, (GrzQDWhere ("obj.siteGuid IN (" ++ (grzObjListToString objList) ++ ")")) : x)
   
hasSite o = hasSites [o]

hasEnabled :: GrzQueryDef -> GrzQueryDef
hasEnabled (n, x) =
    (n, (GrzQDWhere "obj.enabled = 1") : x)

hasDisabled :: GrzQueryDef -> GrzQueryDef 
hasDisabled (n, x) =
    (n, (GrzQDWhere ("obj.enabled = 0")) : x)

hasTypes :: [String] -> GrzQueryDef -> GrzQueryDef  
hasTypes strList (n, x) =
   (n, (GrzQDWhereFrags ([GrzQDString "obj.objectType IN ("] ++ [GrzQDNameList strList] 
        ++ [GrzQDString ")"]))  : x)
        
hasType ot = hasTypes [ot]

hasRels :: [String] -> [GrzObj] -> GrzQueryDef -> GrzQueryDef
hasRels rel objList (n, x) =
    (n, 
        (GrzQDWhereFrags 
            (
                [GrzQDString 
                    ("EXISTS (SELECT guid1 FROM relationships r WHERE "
                        ++ "guid1 = obj.guid AND "
                        ++ "guid2 IN (" ++ (grzObjListToString objList) ++ ") "
                        ++ "AND r.relationshipType IN ("
                    )
                ] 
                ++ [GrzQDNameList rel] 
                ++ [GrzQDString "))"]
            )
        ) : x)
        
hasRel r = hasRels [r]
        
hasInvRels :: [String] -> [GrzObj] -> GrzQueryDef -> GrzQueryDef
hasInvRels rel objList (n, x) =
    (n, (GrzQDWhereFrags ([GrzQDString ("EXISTS (SELECT guid1 FROM "
        ++ "relationships r WHERE "
        ++ "guid2 = obj.guid AND "
        ++ "guid1 IN (" ++ (grzObjListToString objList) ++ ") "
        ++ "AND r.relationshipType IN (")]
        ++ [GrzQDNameList rel] ++ [GrzQDString "))"])) : x)
        
hasInvRel r = hasInvRels [r]
        
hasInvIndRel :: String -> String -> [GrzObj] -> GrzQueryDef -> GrzQueryDef
hasInvIndRel rel1 rel2 objList (n, x) =
    (n, 
        (GrzQDWhereFrags 
            (
                [GrzQDString 
                    ("EXISTS (SELECT r1.guid1 FROM relationships r1 INNER JOIN relationships r2 "
                        ++ "ON (r1.guid2 = r2.guid1) WHERE "
                        ++ "r1.guid1 = obj.guid AND "
                        ++ "r2.guid2 IN (" ++ (grzObjListToString objList) ++ ") "
                        ++ "AND r1.relationshipType = "
                    )
                ] 
                ++ [GrzQDName rel1] 
                ++ [GrzQDString " AND r2.relationshipType = "]
                ++ [GrzQDName rel2]
                ++ [GrzQDString ")"]
            )
        ) : x)
        
hasIndRel :: String -> String -> [GrzObj] -> GrzQueryDef -> GrzQueryDef
hasIndRel rel1 rel2 objList (n, x) =
    (n, 
        (GrzQDWhereFrags 
            (
                [GrzQDString 
                    ("EXISTS (SELECT r1.guid1 FROM relationships r1 INNER JOIN relationships r2 "
                        ++ "ON (r1.guid2 = r2.guid1) WHERE "
                        ++ "r2.guid2 = obj.guid AND "
                        ++ "r1.guid1 IN (" ++ (grzObjListToString objList) ++ ") "
                        ++ "AND r1.relationshipType = "
                    )
                ] 
                ++ [GrzQDName rel1] 
                ++ [GrzQDString " AND r2.relationshipType = "]
                ++ [GrzQDName rel2]
                ++ [GrzQDString ")"]
            )
        ) : x)
   
hasAtomOp :: String -> String -> [GrzAtom] -> GrzQueryDef -> GrzQueryDef   
hasAtomOp name op values (n, x) =
    (n+1,(GrzQDJoin ("INNER JOIN metadata m" ++ (show n) ++ " ON (obj.guid = m" 
        ++ (show n) ++ ".objectGuid)")) 
    : (GrzQDWhereFrags ([GrzQDString ("m" ++ (show n) ++ ".nameId = ")] ++ ([GrzQDName name]) 
        ++ [GrzQDString (" AND ")]
        ++ [getAtomClause values n op]))
    : x)
    
hasAgg :: String -> GrzQueryDef -> GrzQueryDef   
hasAgg name (n, x) =
    (n,(GrzQDJoin ("INNER JOIN metadata ma ON (obj.guid = ma.objectGuid)")) 
    : (GrzQDWhereFrags ([GrzQDString ("ma.nameId = ")] ++ ([GrzQDName name]) 
        ))
    : x)
    
hasSearchable :: String -> GrzQueryDef -> GrzQueryDef   
hasSearchable content (n, x) =
    (n+1,(GrzQDJoin ("INNER JOIN searchable s ON (obj.objectType = s.typeID)")) 
    : (GrzQDJoin ("INNER JOIN metadata m" ++ (show n) ++ " ON (m" ++ (show n) ++ ".nameID = s.nameID)"))
    : (GrzQDWhereFrags ([getAtomClause [stringToAtom content] n "match"]))
    : (GrzQDWhere ("m" ++ (show n) ++ ".objectGuid = obj.guid"))
    : x)

hasAtomIn :: String -> [GrzAtom] -> GrzQueryDef -> GrzQueryDef   
hasAtomIn name values (n, x) = hasAtomOp name "IN" values (n, x)
    
hasStringIn :: String -> [GrzString] -> GrzQueryDef -> GrzQueryDef
hasStringIn name values (n, x) = hasAtomIn name (map stringToAtom values) (n, x)

hasIntIn :: String -> [GrzInt] -> GrzQueryDef -> GrzQueryDef
hasIntIn name values (n, x) = hasAtomIn name (map intToAtom values) (n, x)

hasBoolIn :: String -> [Bool] -> GrzQueryDef -> GrzQueryDef
hasBoolIn name values (n, x) = hasAtomIn name (map boolToAtom values) (n, x)    
        
hasStringBetween :: String -> (GrzString,GrzString) -> GrzQueryDef -> GrzQueryDef
hasStringBetween name (v0,v1) (n, x) = hasAtomOp name "><" (map stringToAtom [v0,v1]) (n, x)

hasIntBetween :: String -> (GrzInt,GrzInt) -> GrzQueryDef -> GrzQueryDef
hasIntBetween name (v0,v1) (n, x) = hasAtomOp name "><" (map intToAtom [v0,v1]) (n, x)

hasStringOp :: String -> String -> GrzString -> GrzQueryDef -> GrzQueryDef   
hasStringOp name op value (n, x) = hasAtomOp name op (map stringToAtom [value]) (n, x)

hasIntOp :: String -> String -> GrzInt -> GrzQueryDef -> GrzQueryDef   
hasIntOp name op value (n, x) = hasAtomOp name op (map intToAtom [value]) (n, x)

withData :: [String] -> GrzQueryDef -> GrzQueryDef
withData vs (n, x) = foldl' (\(n,x) v -> (n+1,(GrzQDNeeds v n):x)) (n,x) vs

-- various simple utility functions

grzObjListToString :: [GrzObj] -> String
-- a bit of a kludge
grzObjListToString [] = "-1"
grzObjListToString x =
    intercalate ", " (map (show . getID) x)

grzSafeAtomListToIntString :: [GrzAtom] -> String
grzSafeAtomListToIntString x =
    intercalate ", " (map (show . safeAtomToInt) x)

-- given a list of atoms, a join number and an operation, generates appropriate where value clauses

getAtomClause :: [GrzAtom] -> Int -> String -> GrzQDWFItem   
getAtomClause atoms n op = 
        GrzQDAtomClause valid intClause boolClause stringClause ints bools strings
    where
        m = "m" ++ (show n)
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
                        else "(" ++ m ++ ".metadataType = 0 AND " ++ (getOpBit op (m ++ ".integerValue") intAtoms) ++ ")"
        boolClause = if null bools 
                        then "" 
                        else "(" ++ m ++ ".metadataType = 1 AND " ++ (getOpBit op (m ++ ".integerValue") boolAtoms)  ++ ")"
        stringClause = if null strings 
                        then "" 
                        else "(" ++ m ++ ".metadataType = 2 AND " ++ (getOpBit op (m ++ ".stringValue") stringAtoms)  ++ ")"

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
        grzAtomListToStrings2 [] (x:xs) end = ((grzTrimComma x ++end): xs)
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