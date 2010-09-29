module Database.Gruze (

    -- defines the public interface for the Gruze object store
    
    -- first the atom, box and object constructors, getters and setters
    
    -- constructors (note that the internals of GrzObj are not exported)
    GrzObj, GrzBox(..), GrzRel(..),
    
    -- classes
    
    GrzObjClass(..), GrzContainerClass(..), GrzOwnerClass(..), GrzSiteClass(..),
    GrzAtomBoxClass(..), 
    
    -- types
    GrzAtom, GrzAtomBox, GrzObjBox, GrzInt, GrzString, GrzKey, GrzLogLevel(..),
    GrzDatabaseType(..),

    -- atom converters
    atomToString, maybeAtomToString, safeAtomToString, forceAtomToString, 
    ppAtom, stringToAtom, isStringAtom,
    atomToInt, maybeAtomToInt, safeAtomToInt, intToAtom, isIntAtom,
    atomToBool, maybeAtomToBool, boolToAtom, isBoolAtom,
    atomToFileID, maybeAtomToFileID, isFileAtom,
    
    -- special object constructors
    emptyObj, emptyBareObj,
    
    -- object setter (setType is only allowed for unwrapped GrzObjs)    
    setType,
    
    -- object type convert    
    maybeConvert, objWrapperToString,
    
    -- object pretty printers
    
    ppObj, ppObjFull,
    
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
    
    -- object query API
    
    -- types
    GrzQueryDef, GrzRelDir(..), GrzRef(..), GrzOrderBy(..),
    
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
    -- by the fixed relationships
    withOwner, withContainer, withSite,
    withOwners, withContainers, withSites,
    
    -- by general relationships
    hasRel,
    
    -- by searchable fields
    hasSearchable,
    
    -- has the specified metadata names defined
    hasData,
    
    -- return objects with the given metadata in the results
    withData,

    -- object IO
    
    -- data handle
    GrzHandle(..), setDefaultSite, setThumbDefs, setLogLevel,
    
    -- utility functions    
    grzLog, grzCommit, grzQuery,
    
    -- query functions
    getObjs, getUnwrappedObjs, getBareObjs, getUnwrappedBareObjs, getObjIDs,
    getObjCount, getObjSumCount, getObjsAggByObjCount, getObjsAggByObjSumCount, 
    setSearchable,
    
    -- create, save, delete, disable, enable and load
    createObj, saveObj, delObj, disableObj, enableObj,
    loadObj, maybeLoadObj, maybeLoadContainer, maybeLoadOwner, maybeLoadSite,

    -- file handler
    createFileAtom, maybeGetFileMetadata, maybeGetFileContent, maybeGetFileThumb,
    
    -- relationship IO
    addRel, delRel, checkRel, hasContainer, hasOwner, hasSite,
    
    -- rexport some basic modules
    
    module Data.Maybe,
    module Data.Typeable,
    module Data.List.Split 
    
) where

import Database.Gruze.Box
import Database.Gruze.IO
import Database.Gruze.QueryDef
import Database.Gruze.Utility
import Database.Gruze.Types

import Data.Maybe
import Data.Typeable
import Data.List.Split