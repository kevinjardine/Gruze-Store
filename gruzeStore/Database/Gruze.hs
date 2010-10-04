module Database.Gruze (

    -- defines the public interface for the Gruze object store
    
    -- * Pure functions for atoms, boxes and objects
    
    -- ** Constructors
    -- (note that the internals of GrzObj are not exported)

    GrzAtomKey(..), GrzObj, GrzBox(..), GrzRel(..),
    
    -- ** Classes
    
    GrzAtomKeyClass(..),
    
    GrzObjClass(getID,unwrapObj,shrinkObj,isValidObj,getType,
        getTimeCreated,getTimeUpdated,getContainer,getOwner,getSite,
        setEnabled, isEnabled, getMetadata, setMetadata), 
        
    GrzContainerClass(..), GrzOwnerClass(..), GrzSiteClass(..),
    GrzAtomBoxClass(..),
    
    -- ** Types
    GrzAtom, GrzAtomBox, 
    GrzAtomIntKey, GrzAtomStringKey, GrzAtomBoolKey, GrzAtomAtomKey,
    GrzObjBox, GrzInt, GrzString, GrzKey, GrzLogLevel(..),
    GrzDatabaseType(..),

    -- ** Atom converters
    -- *** Strings
    atomToString, maybeAtomToString, safeAtomToString, forceAtomToString, 
    ppAtom, stringToAtom, isStringAtom,
    -- *** Ints
    atomToInt, maybeAtomToInt, safeAtomToInt, intToAtom, isIntAtom,
    -- *** Bools
    atomToBool, maybeAtomToBool, boolToAtom, isBoolAtom,
    -- *** Files
    atomToFileID, maybeAtomToFileID, isFileAtom,
    
    -- ** Empty constructors
    emptyAtomBox, emptyObj, emptyBareObj,
    
    -- ** Type setter   
    setType,
    
    -- ** Attempt to wrap object
    maybeConvert,
    
    -- ** Pretty printers    
    ppAtomBox, ppObj, ppObjFull,    
    
    -- ** Atom boxes
    
    addAtomPair, addAtomPairs, removeFromAtomBox, getKeysFromAtomBox,
    
    -- ** Field lists
    fields, noMetadata, allMetadata,
    
    -- ** Object boxes
    setObj, getObj, maybeGetObj, removeFromObjBox, getKeysFromObjBox, 
        
    -- * Query combinators
    
    -- ** Types
    GrzQueryDef, GrzRelDir(..), GrzRef(..), GrzOrderBy(..),
    
    -- ** Classes
    GrzQueryTypeClass(hasIn,hasBetween,hasOp),
    
    -- ** Booleans
    hasTrue, hasFalse,
    
    -- ** Filter by specific objects
    withObj, withObjs,
    
    -- ** Select by type 
    hasType, hasTypes,
    
    -- ** Select by enabled/disabled
    hasEnabled, hasDisabled,
    
    -- ** Select by the fixed relationships
    withOwner, withContainer, withSite,
    withOwners, withContainers, withSites,
    
    -- ** Select by general relationships
    hasRel,
    
    -- ** Select by searchable fields
    hasSearchable,
    
    -- ** Has the specified metadata names defined
    hasData,
    
    -- return objects with the given metadata in the results
    -- withData,

    -- * Object IO
    
    -- ** Data handle
    GrzHandle(..), setDefaultSite, setThumbDefs, setLogLevel,
    
    -- ** File atom handler
    createFileAtom, maybeGetFileMetadata, maybeGetFileContent, maybeGetFileThumb,
    
    -- ** Manage individual objects
    createObj, saveObj, delObj, disableObj, enableObj,
    loadObj, maybeLoadObj, maybeLoadContainer, maybeLoadOwner, maybeLoadSite,
    
    -- ** Query retrieval functions
    getObjs, getUnwrappedObjs, getBareObjs, getUnwrappedBareObjs, getObjIDs,
    getObjCount, getObjSumCount, getObjsAggByObjCount, getObjsAggByObjSumCount, 
    setSearchable,    
    
    -- ** Relationship IO
    addRel, delRel, checkRel, hasContainer, hasOwner, hasSite,
    
    -- ** Utility functions    
    grzLog, grzCommit, grzRollback, grzQuery, grzRunSql,
    
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