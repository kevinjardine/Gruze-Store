module Data.Store.Gruze (

    -- defines the public interface for the Gruze object store
    
    -- first the atom, box and object constructors, getters and setters
    
    -- constructors (note that the internals of GrzObj are not exported)
    GrzObj, GrzBox(..),
    
    -- classes
    
    GrzObjClass(..), GrzContainerClass(..), GrzOwnerClass(..), GrzSiteClass(..),
    GrzAtomBoxClass(..),
    
    -- types
    GrzAtom, GrzAtomBox, GrzObjBox, GrzInt, GrzString, GrzKey, GrzLogLevel(..),

    -- atom converters
    atomToString, maybeAtomToString, safeAtomToString, ppAtom, stringToAtom,
    isStringAtom, atomToInt, maybeAtomToInt, safeAtomToInt, intToAtom, 
    isIntAtom, atomToBool, maybeAtomToBool, boolToAtom, isBoolAtom,
    atomToFileID, maybeAtomToFileID, isFileAtom,
    
    -- special object constructors
    emptyObj, invalidObj,
    
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
    
    -- filter by specific objects
    withObjs, withObj,
    
    -- by type 
    hasTypes, hasType,
    
    -- by enabled/disabled
    hasEnabled, hasDisabled,
    
    -- by the fixed relationships
    hasOwners, hasOwner, hasContainers, hasContainer, hasSites, hasSite,
    
    -- by general relationships
    hasRels, hasRel, hasInvRels, hasInvRel, hasIndRel, hasInvIndRel,
    
    -- by searchable fields
    hasSearchable,
    
    -- by specific name and values
    hasStringIn, hasIntIn, hasBoolIn, hasStringBetween, hasIntBetween,
    hasStringOp, hasIntOp,
    
    -- has the specified names defined
    hasData,
    
    -- return objects with the given metadata in the results
    withData,

    -- object IO
    
    -- data handle
    GrzHandle(..), getHandle, setDefaultSite, setThumbDefs, setLogLevel,
    
    -- utility functions    
    grzLog, grzCommit, grzQuery,
    
    -- query functions
    getObjs, getUnwrappedObjs, getBareObjs, getUnwrappedBareObjs, getObjIDs,
    getObjCount, getObjAggCount, getObjAggSumCount, setSearchable,
    
    -- create, save and load
    createObj, saveObj, delObj,
    loadObj, maybeLoadObj, maybeLoadContainer, maybeLoadOwner, maybeLoadSite,

    -- file handler
    createFileAtom, maybeGetFileMetadata, maybeGetFileContent, maybeGetFileThumb,
    
    -- relationship IO
    addRel, delRel, checkRel
    
) where

import Data.Store.Gruze.Box
import Data.Store.Gruze.IO
import Data.Store.Gruze.QueryDef
import Data.Store.Gruze.Utility
import Data.Store.Gruze.Types