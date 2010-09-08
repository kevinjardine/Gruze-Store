module Data.Store.Gruze.Container (

    -- constructors (note that the internals of GrzObj are not exported)
    GrzObj, GrzBox(..),
    
    -- classes
    
    ToGrzObj(..),
    
    -- types
    GrzAtom, GrzAtomBox, GrzObjBox, GrzInt, GrzString, GrzKey,

    -- atom converters
    atomToString, maybeAtomToString, safeAtomToString, ppAtom, stringToAtom, isStringAtom,
    atomToInt, maybeAtomToInt, safeAtomToInt, intToAtom, isIntAtom,
    atomToBool, maybeAtomToBool, boolToAtom, isBoolAtom,
    atomToFileID, maybeAtomToFileID, isFileAtom,
    
    -- object accessors
    emptyObj, invalidObj, getID, isValidObj, setType, getType,
    getTimeCreated, getTimeUpdated, setOwner, getOwner, setContainer, getContainer, 
    setSite, getSite, setEnabled, isEnabled, shrinkObj,
    
    -- atom box functions
    
    emptyAtomBox,
    
    -- for atoms
    
    setAtom, addAtom, setAtomList, addAtomList, addAtomPair, addAtomPairs, 
    getAtom, getAtomList, maybeGetAtom, maybeGetAtomList, removeFromAtomBox,
    getKeysFromAtomBox,
    
    -- for strings
    
    setString, addString, setStringList, addStringList, getString, 
    maybeGetString, getStringList, maybeGetStringList,
    
    -- for ints
    
    setInt, addInt, setIntList, addIntList, getInt, 
    maybeGetInt, getIntList, maybeGetIntList,
    
    -- for bools
    
    setBool, addBool, setBoolList, addBoolList, getBool, 
    maybeGetBool, getBoolList, maybeGetBoolList,
    
    -- object box functions 

    setObj, getObj, maybeGetObj, removeFromObjBox, getKeysFromObjBox, 
    getMetadata, setMetadata    

) where

import Data.Store.Gruze.Types
import qualified Data.Map as Map
import Data.List (foldl')
import Data.Maybe
    
-- this is only defined for string atoms
-- anything else will cause a run time error
atomToString :: GrzAtom -> GrzString  
atomToString (GrzAtomString v) = v

-- a safer maybe version
maybeAtomToString :: GrzAtom -> Maybe GrzString  
maybeAtomToString (GrzAtomString v) = Just v
maybeAtomToString _ = Nothing

-- another safe version that converts all but strings to the empty string
safeAtomToString :: GrzAtom -> GrzString  
safeAtomToString (GrzAtomInt a) = ""
safeAtomToString (GrzAtomBool True) = ""
safeAtomToString (GrzAtomBool False) = ""
safeAtomToString (GrzAtomString a) = a
safeAtomToString (GrzAtomFile a) = ""

-- another safe version that converts everything    
ppAtom :: GrzAtom -> GrzString  
ppAtom (GrzAtomInt a) = show a
ppAtom (GrzAtomBool True) = "True"
ppAtom (GrzAtomBool False) = "False"
ppAtom (GrzAtomString a) = "\"" ++ a ++ "\""
ppAtom (GrzAtomFile a) = "File " ++ (show a)

stringToAtom :: GrzString -> GrzAtom
stringToAtom s = GrzAtomString s

isStringAtom :: GrzAtom -> Bool
isStringAtom (GrzAtomString _) = True
isStringAtom _ = False

-- this is only defined for boolean atoms
-- anything else will cause a run time error
atomToBool :: GrzAtom -> Bool  
atomToBool (GrzAtomBool v) = v

-- a safer maybe version
maybeAtomToBool :: GrzAtom -> Maybe Bool  
maybeAtomToBool (GrzAtomBool v) = Just v
maybeAtomToBool _ = Nothing

boolToAtom :: Bool -> GrzAtom
boolToAtom b = GrzAtomBool b

isBoolAtom :: GrzAtom -> Bool
isBoolAtom (GrzAtomBool _) = True
isBoolAtom _ = False

-- this is only defined for int atoms
-- anything else will cause a run time error
atomToInt :: GrzAtom -> GrzInt 
atomToInt (GrzAtomInt v) = v

-- a safer maybe version
maybeAtomToInt :: GrzAtom -> Maybe GrzInt  
maybeAtomToInt (GrzAtomInt v) = Just v
maybeAtomToInt _ = Nothing

-- another safe version that converts everything
safeAtomToInt :: GrzAtom -> Int  
safeAtomToInt (GrzAtomInt a) = a
safeAtomToInt (GrzAtomFile a) = a
safeAtomToInt (GrzAtomBool True) = 1
safeAtomToInt (GrzAtomBool False) = 0
safeAtomToInt (GrzAtomString a) = 0

intToAtom :: GrzInt -> GrzAtom
intToAtom i = GrzAtomInt i

isIntAtom :: GrzAtom -> Bool
isIntAtom (GrzAtomInt _) = True
isIntAtom _ = False

-- this is only defined for file atoms
-- anything else will cause a run time error
atomToFileID :: GrzAtom -> GrzInt
atomToFileID (GrzAtomFile v) = v

-- a safer maybe version
maybeAtomToFileID :: GrzAtom -> Maybe GrzInt  
maybeAtomToFileID (GrzAtomFile v) = Just v
maybeAtomToFileID _ = Nothing

-- not sure if fileIDToAtom is needed and might even be dangerous
-- as file atoms should not be forged
-- fileIDToAtom :: GrzInt -> GrzAtom
-- fileIDToAtom i = GrzAtomFile i

isFileAtom :: GrzAtom -> Bool
isFileAtom (GrzAtomFile _) = True
isFileAtom _ = False

-- * Gruze atom box getters and setters

setAtom :: GrzAtomBoxClass c => GrzKey -> GrzAtom -> c -> c   
setAtom k a c = putAtomBox (Map.insert k [a] (getAtomBox c)) c

addAtom :: GrzAtomBoxClass c => GrzKey -> GrzAtom -> c -> c
addAtom k a c = putAtomBox (Map.insert k ((fromMaybe [] (Map.lookup k (getAtomBox c))) ++ [a]) (getAtomBox c)) c

setAtomList :: GrzAtomBoxClass c => GrzKey -> [GrzAtom] -> c -> c
setAtomList k as c = putAtomBox (Map.insert k as (getAtomBox c)) c

addAtomList :: GrzAtomBoxClass c => GrzKey -> [GrzAtom] -> c -> c
addAtomList k a c = putAtomBox (Map.insert k ((fromMaybe [] (Map.lookup k (getAtomBox c))) ++ a) (getAtomBox c)) c

addAtomPair :: GrzAtomBoxClass c => (GrzKey, [GrzAtom]) -> c -> c
addAtomPair (k, as) c = addAtomList k as c

addAtomPairs ps c = foldl' (flip addAtomPair) c ps

getAtom :: GrzAtomBoxClass c => GrzKey -> GrzAtom -> c -> GrzAtom
getAtom k d c = head $ fromMaybe [d] (Map.lookup k (getAtomBox c))

getAtomList :: GrzAtomBoxClass c => GrzKey -> [GrzAtom] -> c -> [GrzAtom]
getAtomList k d c = fromMaybe d (Map.lookup k (getAtomBox c))

maybeGetAtom :: GrzAtomBoxClass c => GrzKey -> c -> Maybe GrzAtom
maybeGetAtom k c = case Map.lookup k (getAtomBox c) of
                        Nothing -> Nothing
                        Just [] -> Nothing
                        Just x -> Just (head x)
                        
maybeGetAtomList :: GrzAtomBoxClass c => GrzKey -> c -> Maybe [GrzAtom]                        
maybeGetAtomList k c = Map.lookup k (getAtomBox c)

removeFromAtomBox :: GrzAtomBoxClass c => GrzKey -> c -> c
removeFromAtomBox k c = putAtomBox (Map.delete k (getAtomBox c)) c

getKeysFromAtomBox :: GrzAtomBoxClass c => c -> [GrzKey]
getKeysFromAtomBox c = Map.keys (getAtomBox c)

-- string getters and setters
    
setString :: GrzAtomBoxClass c => GrzKey -> GrzString -> c -> c
setString k s c = setAtom k (stringToAtom s) c

addString :: GrzAtomBoxClass c => GrzKey -> GrzString -> c -> c
addString k s c = addAtom k (stringToAtom s) c

setStringList :: GrzAtomBoxClass c => GrzKey -> [GrzString] -> c -> c
setStringList k ss c = setAtomList k (map stringToAtom ss) c

addStringList :: GrzAtomBoxClass c => GrzKey -> [GrzString] -> c -> c
addStringList k ss c = addAtomList k (map stringToAtom ss) c

getString :: GrzAtomBoxClass c => GrzKey -> GrzString -> c -> GrzString
getString k d c = 
    if isStringAtom r then atomToString r else d
        where
            r = getAtom k (stringToAtom d) c
            
maybeGetString :: GrzAtomBoxClass c => GrzKey -> c -> Maybe GrzString
maybeGetString k c = 
    case maybeGetAtom k c of
        Nothing -> Nothing
        Just a -> if isStringAtom a
                    then
                        Just (atomToString a)
                    else
                        Nothing                

getStringList :: GrzAtomBoxClass c => GrzKey -> [GrzString] -> c -> [GrzString]
getStringList k d c = 
    if and $ map isStringAtom r then map atomToString r else d
        where
            r = getAtomList k (map stringToAtom d) c
            
maybeGetStringList :: GrzAtomBoxClass c => GrzKey -> c -> Maybe [GrzString]
maybeGetStringList k c = 
    case maybeGetAtomList k c of
        Nothing -> Nothing
        Just a -> if and $ map isStringAtom a
                    then
                        Just (map atomToString a)
                    else
                        Nothing
                        
-- int getters and setters
    
setInt :: GrzAtomBoxClass c => GrzKey -> GrzInt -> c -> c
setInt k s c = setAtom k (intToAtom s) c

addInt :: GrzAtomBoxClass c => GrzKey -> GrzInt -> c -> c
addInt k s c = addAtom k (intToAtom s) c

setIntList :: GrzAtomBoxClass c => GrzKey -> [GrzInt] -> c -> c
setIntList k ss c = setAtomList k (map intToAtom ss) c

addIntList :: GrzAtomBoxClass c => GrzKey -> [GrzInt] -> c -> c
addIntList k ss c = addAtomList k (map intToAtom ss) c

getInt :: GrzAtomBoxClass c => GrzKey -> GrzInt -> c -> GrzInt
getInt k d c = 
    if isIntAtom r then atomToInt r else d
        where
            r = getAtom k (intToAtom d) c
            
maybeGetInt :: GrzAtomBoxClass c => GrzKey -> c -> Maybe GrzInt
maybeGetInt k c = 
    case maybeGetAtom k c of
        Nothing -> Nothing
        Just a -> if isIntAtom a
                    then
                        Just (atomToInt a)
                    else
                        Nothing                

getIntList :: GrzAtomBoxClass c => GrzKey -> [GrzInt] -> c -> [GrzInt]
getIntList k d c = 
    if and $ map isIntAtom r then map atomToInt r else d
        where
            r = getAtomList k (map intToAtom d) c
            
maybeGetIntList :: GrzAtomBoxClass c => GrzKey -> c -> Maybe [GrzInt]
maybeGetIntList k c = 
    case maybeGetAtomList k c of
        Nothing -> Nothing
        Just a -> if and $ map isIntAtom a
                    then
                        Just (map atomToInt a)
                    else
                        Nothing
                        
-- bool getters and setters
    
setBool :: GrzAtomBoxClass c => GrzKey -> Bool -> c -> c
setBool k s c = setAtom k (boolToAtom s) c

addBool :: GrzAtomBoxClass c => GrzKey -> Bool -> c -> c
addBool k s c = addAtom k (boolToAtom s) c

setBoolList :: GrzAtomBoxClass c => GrzKey -> [Bool] -> c -> c
setBoolList k ss c = setAtomList k (map boolToAtom ss) c

addBoolList :: GrzAtomBoxClass c => GrzKey -> [Bool] -> c -> c
addBoolList k ss c = addAtomList k (map boolToAtom ss) c

getBool :: GrzAtomBoxClass c => GrzKey -> Bool -> c -> Bool
getBool k d c = 
    if isBoolAtom r then atomToBool r else d
        where
            r = getAtom k (boolToAtom d) c
            
maybeGetBool :: GrzAtomBoxClass c => GrzKey -> c -> Maybe Bool
maybeGetBool k c = 
    case maybeGetAtom k c of
        Nothing -> Nothing
        Just a -> if isBoolAtom a
                    then
                        Just (atomToBool a)
                    else
                        Nothing                

getBoolList :: GrzAtomBoxClass c => GrzKey -> [Bool] -> c -> [Bool]
getBoolList k d c = 
    if and $ map isBoolAtom r then map atomToBool r else d
        where
            r = getAtomList k (map boolToAtom d) c
            
maybeGetBoolList :: GrzAtomBoxClass c => GrzKey -> c -> Maybe [Bool]
maybeGetBoolList k c = 
    case maybeGetAtomList k c of
        Nothing -> Nothing
        Just a -> if and $ map isBoolAtom a
                    then
                        Just (map atomToBool a)
                    else
                        Nothing
                        
-- TODO: file getters and setters
-- as files cannot be forged, not sure how to do that yet
    
-- * Gruze object getters and setters

invalidObj = GrzObjID 0
emptyObj = GrzObjFull 0 "" 0 0 invalidObj invalidObj invalidObj True Map.empty

getID :: GrzObj -> Int        
getID (GrzObjID x) = x
getID obj = objID obj

isValidObj :: GrzObj -> Bool
isValidObj obj = (getID obj) > 0

setType :: GrzString -> GrzObj -> GrzObj
setType t obj = obj { objType = t }

getType :: GrzObj -> GrzString
getType obj = objType obj

getTimeCreated :: GrzObj -> GrzInt
getTimeCreated obj = objTimeCreated obj

getTimeUpdated :: GrzObj -> GrzInt
getTimeUpdated obj = objTimeUpdated obj
        
setOwner :: GrzObj -> GrzObj -> GrzObj
setOwner owner obj = obj { objOwner = owner }

getOwner :: GrzObj -> GrzObj
getOwner obj = objOwner obj

setContainer :: GrzObj -> GrzObj -> GrzObj
setContainer container obj = obj { objContainer = container }

getContainer :: GrzObj -> GrzObj
getContainer obj = objContainer obj

setSite :: GrzObj -> GrzObj -> GrzObj
setSite site obj = obj { objSite = site }

getSite :: GrzObj -> GrzObj
getSite obj = objSite obj

setEnabled :: Bool -> GrzObj -> GrzObj
setEnabled state obj = obj { objEnabled = state }

isEnabled :: GrzObj -> Bool
isEnabled obj = objEnabled obj

getMetadata :: GrzObj -> GrzAtomBox
getMetadata obj = objMetadata obj

setMetadata :: GrzObj -> GrzAtomBox -> GrzObj
setMetadata obj b = obj { objMetadata = b }

-- * Gruze object box getters and setters
    
setObj :: GrzObjBoxClass c => GrzKey -> GrzObj -> c -> c    
setObj k obj c = putObjBox (Map.insert k obj (getObjBox c)) c

getObj :: GrzObjBoxClass c => GrzKey -> GrzObj -> c -> GrzObj
getObj k d c = fromMaybe d (Map.lookup k (getObjBox c))

maybeGetObj :: GrzObjBoxClass c => GrzKey -> c -> Maybe GrzObj
maybeGetObj k c = Map.lookup k (getObjBox c)

removeFromObjBox :: GrzObjBoxClass c => GrzKey -> c -> c
removeFromObjBox k c = putObjBox (Map.delete k (getObjBox c)) c

getKeysFromObjBox :: GrzObjBoxClass c => c -> [GrzKey]
getKeysFromObjBox c = Map.keys (getObjBox c)

shrinkObj :: GrzObj -> GrzObj
shrinkObj obj = GrzObjID (getID obj)