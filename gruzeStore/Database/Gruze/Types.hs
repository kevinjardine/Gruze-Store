{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Gruze.Types where

import qualified Data.Map as Map
import Data.Typeable
import Database.HDBC

-- * Basic types

type GrzKey = String
type GrzString = String
type GrzInt = Int

type GrzQuery = (((String,GrzQueryType),[(String,Int)]), ([String],GrzQueryValues))

data GrzQueryType = GrzQTFull | GrzQTCount | GrzQTID | GrzQTAggCount 
    | GrzQTAggSumCount | GrzQTAggByObjCount Int | GrzQTAggByObjSumCount Int Int
    deriving (Show, Eq)

type GrzQueryDef = ((Int,Int), [GrzQueryDefItem])

data GrzQueryDefItem = 
        GrzQDIsCount Bool 
        | GrzQDWhere Int String 
        | GrzQDJoin Int String 
        | GrzQDWhereFrags Int [GrzQDWFItem]
        | GrzQDOrderBy Int String
        | GrzQDGroupBy Int String
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
    
-- * Gruze atom box key definitions

data GrzAtomKey a = GrzAtomKey { 
    atomKey :: String
}
    deriving (Eq, Show)

class GrzAtomKeyClass t where
    set :: GrzAtomBoxClass b => GrzAtomKey t -> t -> b -> b                 -- ^ replaces the current value with a single value (or creates a new one)
    add :: GrzAtomBoxClass b => GrzAtomKey t -> t -> b -> b                 -- ^ appends to the current values (or creates a new one)
    get :: GrzAtomBoxClass b => GrzAtomKey t -> t -> b -> t                 -- ^ gets the current value (the first one if there are several defined for this field) or returns the default value supplied
    maybeGet :: GrzAtomBoxClass b => GrzAtomKey t -> b -> Maybe t           -- ^ gets the current value (the first one if there are several defined for this field) or returns Nothing
    setList :: GrzAtomBoxClass b => GrzAtomKey t -> [t] -> b -> b           -- ^ replaces the current value with a list (or creates a new one)
    addList :: GrzAtomBoxClass b => GrzAtomKey t -> [t] -> b -> b           -- ^ appends the list to the current values (or creates a new one)
    getList :: GrzAtomBoxClass b => GrzAtomKey t -> [t] -> b -> [t]         -- ^ gets the current list of values for this field or returns the default list supplied
    maybeGetList :: GrzAtomBoxClass b => GrzAtomKey t -> b -> Maybe [t]     -- ^ gets the current list of values for this field or returns Nothing

type GrzAtomIntKey = GrzAtomKey Int
type GrzAtomStringKey = GrzAtomKey String
type GrzAtomBoolKey = GrzAtomKey Bool
type GrzAtomAtomKey = GrzAtomKey GrzAtom
   
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
    
-- Gruze relationships

data GrzRel = GrzRel { getGrzRelString :: String }
    
data GrzRelDir = FwdRel | InvRel
    deriving Eq
    
data GrzLogLevel = DebugLogLevel | NotificationLogLevel | WarningLogLevel | FatalLogLevel
    deriving (Eq, Ord)
    
data GrzRef =  ObjRef | ContainerRef | OwnerRef | SiteRef
    deriving Eq
    
data GrzOrderBy = GuidAsc | GuidDesc | TimeCreatedAsc | TimeCreatedDesc
        | TimeUpdatedAsc | TimeUpdatedDesc | StringAsc (GrzAtomKey String) | StringDesc (GrzAtomKey String)
        | IntAsc (GrzAtomKey Int) | IntDesc (GrzAtomKey Int)
        | CountAsc | CountDesc | SumAsc | SumDesc        
-- TODO: add | AvgAsc | AvgDesc
    deriving (Eq,Show)

data GrzDatabaseType = GrzSqlite3DB | GrzMySQLDB | GrzPostgreSQLDB

data GrzHandle = forall a. IConnection a => GrzHandle {
    grzDatabaseHandle ::  a,
    grzDataDirectory :: FilePath,
    grzConvertLocation :: FilePath,
    grzLogFile :: FilePath,
    grzDefaultSite :: GrzObj,
    grzThumbDefs :: [(String,String)],
    grzLogLevel :: GrzLogLevel,
    grzDatabaseType :: GrzDatabaseType
}

data GrzQueryLocation = Exterior | Interior
