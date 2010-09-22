{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Store.Gruze.Types where

import qualified Data.Map as Map
import Data.Typeable

-- * Basic types

type GrzKey = String
type GrzString = String
type GrzInt = Int

type GrzQuery = (((String,GrzQueryType),[(String,Int)]), ([String],GrzQueryValues))

data GrzQueryType = GrzQTFull | GrzQTCount | GrzQTID | GrzQTAggCount | GrzQTAggSumCount
    deriving (Show, Eq)

type GrzQueryDef = ((Int,Int), [GrzQueryDefItem])

data GrzQueryDefItem = 
        GrzQDIsCount Bool 
        | GrzQDWhere String 
        | GrzQDJoin String 
        | GrzQDWhereFrags [GrzQDWFItem]
        | GrzQDNeeds String Int
        | GrzQDSelect String
        | GrzQDGroup String
        | GrzQDType GrzQueryType
        | GrzQDAgg String

data GrzQDWFItem =
        GrzQDName String
        | GrzQDNameList [String]
        | GrzQDString String
        | GrzQDAtomClause Bool String String String [Int] [Int] [String]
        
type GrzQueryValues = ([Int], [String])

-- TODO: need to rethink the basic type approach - perhaps standardise on ByteString and Int for now?
-- another option might be to use a class, especially for strings

-- toGrzKey :: String -> GrzKey
-- toGrzKey = id

-- toGrzString :: String -> GrzString
-- toGrzString = id

-- toGrzInt :: Int -> GrzInt
-- toGrzInt = id

-- * Gruze atom definition

data GrzAtom = 
    GrzAtomInt GrzInt 
    | GrzAtomBool Bool 
    | GrzAtomString GrzString 
    | GrzAtomFile GrzInt 
    deriving (Show, Ord, Eq, Read)
       
-- * Gruze atom box definition
    
type GrzAtomBox = Map.Map GrzKey [GrzAtom]

emptyAtomBox = Map.empty

class GrzAtomBoxClass c where
    getAtomBox :: c -> GrzAtomBox
    putAtomBox :: GrzAtomBox -> c -> c
    
instance GrzAtomBoxClass GrzAtomBox where
    getAtomBox c = c
    putAtomBox b _ = b
    
-- * Gruze object definition
    
data GrzObj = GrzObjID GrzInt |
    GrzObjFull {
        objID :: GrzInt,
        objType :: GrzString,
        objTimeCreated :: GrzInt,
        objTimeUpdated :: GrzInt,
        objOwner :: GrzObj,
        objContainer :: GrzObj,
        objSite :: GrzObj,
        objEnabled :: Bool,
        objMetadata :: GrzAtomBox
    }
    deriving (Read, Show, Typeable)
       
instance GrzAtomBoxClass GrzObj where
    getAtomBox c = objMetadata c
    putAtomBox b c = c { objMetadata = b } 
    
-- * Gruze object box definition
    
type GrzObjBox = Map.Map GrzKey GrzObj

class GrzObjBoxClass c where
    getObjBox :: c -> GrzObjBox
    putObjBox :: GrzObjBox -> c -> c
    
instance GrzObjBoxClass GrzObjBox where
    getObjBox c = c
    putObjBox b _ = b
    
-- * Gruze box definition
    
data GrzBox = GrzBox {
    atomBox :: GrzAtomBox,
    objBox :: GrzObjBox
}

instance GrzAtomBoxClass GrzBox where
    getAtomBox c = atomBox c
    putAtomBox b c = c { atomBox = b }
    
instance GrzObjBoxClass GrzBox where
    getObjBox c = objBox c
    putObjBox b c = c { objBox = b }
    
data GrzLogLevel = DebugLogLevel | NotificationLogLevel | WarningLogLevel | FatalLogLevel
    deriving (Eq, Ord)
    
data GrzRelDir = ForwardRel | BackwardRel
    deriving Eq
    
data GrzRef =  ObjRef | ContainerRef | OwnerRef | SiteRef
    deriving Eq
 