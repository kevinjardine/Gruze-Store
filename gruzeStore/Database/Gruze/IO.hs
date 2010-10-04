{-# LANGUAGE ExistentialQuantification #-}
module Database.Gruze.IO (
    GrzHandle(..), 
    
    grzLog, grzCommit, setDefaultSite, setThumbDefs, setLogLevel,
    
    -- query functions
    getObjs, getUnwrappedObjs, getBareObjs, getUnwrappedBareObjs, getObjIDs,
    getObjCount, getObjSumCount, getObjsAggByObjCount, getObjsAggByObjSumCount, 
    setSearchable,
    
    -- object IO
    createObj, saveObj, delObj, disableObj, enableObj,
    loadObj, maybeLoadObj, maybeLoadContainer, maybeLoadOwner, maybeLoadSite,

    -- file handler
    createFileAtom, maybeGetFileMetadata, maybeGetFileContent, maybeGetFileThumb,
    
    -- relationship IO
    addRel, delRel, checkRel, hasContainer, hasOwner, hasSite
) where

-- functions to manage objects, string handles and file handles

import Database.Gruze.Box
import Database.Gruze.Types
import Database.Gruze.QueryDef
import Database.Gruze.Handles
import Database.Gruze.Query
import Database.Gruze.Utility

import Database.HDBC
-- import Database.HDBC.ODBC
--import Database.HDBC.Sqlite3
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Time.Clock.POSIX
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import qualified Data.Map as Map
--import Control.Monad (forM, when)
--import Control.Monad.Reader(liftIO,ask)
import Data.Maybe
import Data.Typeable

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B

import Control.Monad (forM)

-- TODO: put constraints in object table to make sure that the owner, container and site IDs
-- can only be existing objects or 0

-- database API

-- some useful pure functions

atomToStorageInt :: GrzAtom -> Int
atomToStorageInt (GrzAtomInt i) = 0
atomToStorageInt (GrzAtomBool b) = 1
atomToStorageInt (GrzAtomString s) = 2
atomToStorageInt (GrzAtomFile s) = 3

getHandleDict :: Maybe GrzQuery -> [(String,Int)]
getHandleDict (Just (((query,queryType),handleDict),(needs,values))) = handleDict
getHandleDict _ = []

-- the Object IO functions

-- handle functions
-- | Set default site in handle.    
setDefaultSite :: GrzSiteClass os => 
    os              -- ^ site
    -> GrzHandle    -- ^ current handle
    -> GrzHandle    -- ^ new handle
setDefaultSite site grzH  =               
    grzH {grzDefaultSite = unwrapObj site}

-- | Configure image thumbnail settings.    
setThumbDefs :: 
    [(String,String)]   -- ^ list of (size,param) pairs
    -> GrzHandle        -- ^ current handle
    -> GrzHandle        -- ^ new handle
setThumbDefs td grzH =               
    grzH {grzThumbDefs = td}

-- | Set Gruze logging level.      
setLogLevel ::
    GrzLogLevel     -- ^ log level
    -> GrzHandle    -- ^ current handle
    -> GrzHandle    -- ^ new handle
setLogLevel level grzH =               
    grzH {grzLogLevel = level}
        
-- metadata handler functions
        
-- TODO: the next function seems redundant - just replace it with grzInsertMetadata
grzAddMetadata :: GrzHandle -> Int -> Int -> (String,[GrzAtom]) -> IO Integer
grzAddMetadata grzH guid h (s,atoms) = grzInsertMetadata grzH guid h atoms
            
grzInsertMetadata :: GrzHandle -> Int -> Int -> [GrzAtom] -> IO Integer
grzInsertMetadata grzH guid h (a:as) =
    do
        grzRunSql grzH query $ (map toSql [atomToStorageInt a,guid,h,safeAtomToInt a]) ++ [toSql $ safeAtomToString a]
        grzInsertMetadata grzH guid h as
    where
        query = "INSERT INTO metadata (metadataType,objectGuid,nameId,integerValue,stringValue) values(?,?,?,?,?)" 
grzInsertMetadata _ _ _ [] = return 0

grzDeleteMetadata :: GrzHandle -> Int -> Int -> IO ()
grzDeleteMetadata grzH guid h =
    do
        grzRunSql grzH query $ (map toSql [guid,h])
    where
        query = "DELETE FROM metadata WHERE objectGuid = ? AND nameId = ?"

-- TODO: rewrite this using mapM or forM
grzAddMetadataArray :: GrzHandle -> Int -> [(String,[GrzAtom])] -> IO Integer 
grzAddMetadataArray grzH guid (d:ds) =
    do
        h <- getStringHandle grzH (fst d)
        grzAddMetadata grzH guid h d
        grzAddMetadataArray grzH guid ds
grzAddMetadataArray _ _ [] = return 0

-- TODO: rewrite this using mapM or forM
grzSetMetadataArray :: GrzHandle -> Int -> [(String,[GrzAtom])] -> IO Integer 
grzSetMetadataArray grzH guid (d:ds) =
    do
        h <- getStringHandle grzH (fst d)
        grzDeleteMetadata grzH guid h
        grzAddMetadata grzH guid h d
        grzSetMetadataArray grzH guid ds
grzSetMetadataArray _ _ [] = return 0

-- higher level object functions

{-| Takes a type constructor and a setter function
    and saves the object to the datebase, returning the new wrapped object.
-} 
createObj :: (Typeable o, GrzObjClass o) => 
    GrzHandle               -- ^ data handle
    -> (GrzObj -> o)        -- ^ type wrapper
    -> (GrzObj -> GrzObj)   -- ^ setter functions
    -> IO o                 -- ^ new object
createObj grzH w p =
    if null t then 
        return (w emptyObj)
    else do
        ptime <- getPOSIXTime
        let time = floor ptime
        h <- getStringHandle grzH t
        grzRunSql grzH query $ map toSql [h,ownerID,containerID,siteID,time,time,enabled]
        guid <- getLastInsertId grzH
        let theObj = obj {
            objID = guid,
            objType = t,
            objTimeCreated = time, 
            objTimeUpdated = time,
            objOwner = shrinkObj $ getOwner obj,
            objContainer = shrinkObj $ getContainer obj,
            objSite = shrinkObj $ getSite obj
        }
        grzLog grzH DebugLogLevel $ "in createObj, created the object " ++ (ppObjFull theObj)

        -- TODO: perhaps f could return an error condition triggering a rollback?
        f <- grzAddMetadataArray grzH guid (Map.toList $ objMetadata obj)
        grzCommit grzH
        return $ w theObj
    where
        obj = p emptyObj
        enabled = if isEnabled obj then 1 else 0
        siteID = getID $ getSite obj
        ownerID = getID $ getOwner obj
        containerID = getID $ getContainer obj
        t = objWrapperToString (w emptyObj) 
        query = "INSERT INTO objects(objectType,ownerGuid,containerGuid,siteGuid,timeCreated,timeUpdated,enabled) values(?,?,?,?,?,?,?)"         

{-|
  Loads the current data for an object from the database,
  including the specified metadata.
-}        
loadObj :: GrzObjClass o => 
    GrzHandle       -- ^ data handle 
    -> o            -- ^ object to refresh
    -> [String]     -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> IO o         -- ^ The returned object
loadObj grzH obj needs =
    do
        objs <- getUnwrappedObjs grzH qd needs [] 1 0
        return $ case objs of
                    [] -> replaceObj obj emptyObj
                    a  -> replaceObj obj (head a)
    where
        qd = (withObjs [obj])

-- | Reloads an unwrapped object as a wrapped one.     
maybeLoadObj :: GrzObjClass o => 
    GrzHandle           -- ^ data handle
    -> (GrzObj -> o)    -- ^ type wrapper
    -> GrzObj           -- ^ The unwrapped object to load
    -> [String]         -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> IO (Maybe o)     -- ^ The returned object
maybeLoadObj grzH w obj needs =
    if isValidObj obj
        then do
            o2 <- loadObj grzH (w obj) needs
            if isValidObj o2
                then
                    return $ maybeConvert w (unwrapObj o2)
                else
                    return Nothing           
        else
            return Nothing

-- | Loads the container for the given object if it exists.
maybeLoadContainer :: (GrzObjClass o, GrzObjClass oc, GrzContainerClass oc) => 
    GrzHandle           -- ^ data handle
    -> (GrzObj -> oc)   -- ^ expected type wrapper for container
    -> o                -- ^ object which we are returning the container for
    -> [String]         -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> IO (Maybe oc)    -- ^ maybe returns the container
    
maybeLoadContainer grzH w obj needs = maybeLoadObj grzH w (getContainer obj) needs

-- | Loads the owner for the given object if it exists.
maybeLoadOwner :: (GrzObjClass o, GrzOwnerClass oo) => 
    GrzHandle           -- ^ data handle
    -> (GrzObj -> oo)   -- ^ expected type wrapper for owner
    -> o                -- ^ object which we are returning the owner for
    -> [String]         -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> IO (Maybe oo)    -- ^ maybe returns the owner
    
maybeLoadOwner grzH w obj needs = maybeLoadObj grzH w (getOwner obj) needs

-- | Loads the site for the given object if it exists.
maybeLoadSite :: (GrzObjClass o, GrzSiteClass os) => 
    GrzHandle           -- ^ data handle
    -> (GrzObj -> os)   -- ^ expected type wrapper for site
    -> o                -- ^ object which we are returning the site for
    -> [String]         -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> IO (Maybe os)    -- ^ maybe returns the site
    
maybeLoadSite grzH w obj needs = maybeLoadObj grzH w (getSite obj) needs

-- TODO: need some exception handling here
-- | Saves the given object data to the database and resets the timeUpdated field.   
saveObj :: GrzObjClass o => 
    GrzHandle   -- ^ data handle
    -> o        -- ^ object to save   
    -> IO o     -- ^ updated object
saveObj grzH o =
    do 
        ptime <- getPOSIXTime
        let time = floor ptime
        grzRunSql grzH query $ map toSql [ownerID,containerID,siteID,time,guid]
        let theObj = obj {objTimeUpdated = time}
        -- TODO: perhaps f could return an error condition triggering a rollback?
        f <- grzSetMetadataArray grzH guid (Map.toList $ getMetadata theObj)
        grzCommit grzH
        return $ replaceObj o theObj
    where
        obj = unwrapObj o
        guid = getID obj
        enabled = if isEnabled obj then 1 else 0
        siteID = getID $ getSite obj
        ownerID = getID $ getOwner obj
        containerID = getID $ getContainer obj
        query = "UPDATE objects SET ownerGuid = ?, containerGuid = ?, siteGuid = ?, timeUpdated = ? WHERE guid = ?"

{-|
  Deletes all the object data including metadata and relationships and recursively
  deletes all the objects that have this object as a container, owner or site.
-}
delObj :: GrzObjClass o => 
    GrzHandle   -- ^ data handle
    -> o        -- ^ object to delete
    -> IO Bool  -- ^ success status
delObj grzH obj = do
    r <- delObjByID grzH (getID obj)
    if r
        then do
            grzCommit grzH
            return r
        else do
            grzRollback grzH
            return r

delObjByID :: GrzHandle -> Int -> IO Bool
delObjByID grzH 0 = return True
delObjByID grzH guid =
    do
        -- delete the metadata 
        grzRunSql grzH metadata_query $ [toSql guid]
        
        -- delete the relationships
        grzRunSql grzH relationship_query $ [toSql guid,toSql guid]
        
        -- delete the owned objects
        owned <- getObjIDs grzH (withOwners [GrzObjID guid]) [] 0 0
        mapM_ (delObjByID grzH) owned
        
        -- delete the contained objects
        contained <- getObjIDs grzH (withContainers [GrzObjID guid]) [] 0 0
        mapM_ (delObjByID grzH) contained
        
        -- delete the objects that have this object as a site
        sited <- getObjIDs grzH (withSites [GrzObjID guid]) [] 0 0
        mapM_ (delObjByID grzH) sited
        
        -- delete the object from the object table
        grzRunSql grzH object_query $ [toSql guid]
        
        return True
    where
        metadata_query = "DELETE FROM metadata WHERE objectGuid = ?"
        relationship_query = "DELETE FROM relationships WHERE guid1 = ? OR guid2 = ?"
        object_query = "DELETE FROM objects WHERE guid = ?"
        
setEnableObj :: GrzObjClass o => GrzHandle -> Bool -> o -> IO Bool
setEnableObj grzH state obj  = do
    r <- setEnableObjByID grzH (if state then 1 else 0) (getID obj) 
    if r
        then do
            grzCommit grzH
            return r
        else
            return r

setEnableObjByID :: GrzHandle -> Int -> Int -> IO Bool
setEnableObjByID grzH _ 0 = return True
setEnableObjByID grzH state guid  =
    do
        
        -- set the enable state on the owned objects
        owned <- getObjIDs grzH (withOwners [GrzObjID guid]) [] 0 0
        mapM_ (setEnableObjByID grzH state) owned
        
        -- set the enable state on the contained objects
        contained <- getObjIDs grzH (withContainers [GrzObjID guid]) [] 0 0
        mapM_ (setEnableObjByID grzH state) contained
        
        -- set the enable state on the objects that have this object as a site
        sited <- getObjIDs grzH (withSites [GrzObjID guid]) [] 0 0
        mapM_ (setEnableObjByID grzH state) sited
        
        -- set the enable state on the object itself
        grzQuery grzH object_query $ [toSql state, toSql guid]       
        
        return True
    where
        object_query = "UPDATE objects SET enabled = ? WHERE guid = ?"

{-|
  Disables the object and recursively disables all the objects that
  have this object as a container, owner or site.
-}
disableObj :: GrzObjClass o => 
    GrzHandle   -- ^ data handle
    -> o        -- ^ object to disable
    -> IO Bool  -- ^ success status   
disableObj grzH obj = setEnableObj grzH False obj

{-|
  Enables the object and recursively enables all the objects that
  have this object as a container, owner or site.
-}
enableObj :: GrzObjClass o => 
    GrzHandle   -- ^ data handle
    -> o        -- ^ object to enable
    -> IO Bool  -- ^ success status     
enableObj grzH obj = setEnableObj grzH True obj
        
-- relationship functions                  

-- | Adds the relationship to the given objects (if it does not exist).
addRel :: (GrzObjClass o1, GrzObjClass o2) => 
    GrzHandle   -- ^ data handle
    -> GrzRel   -- ^ relationship
    -> o1       -- ^ first object
    -> o2       -- ^ second object
    -> IO ()
addRel grzH (GrzRel rel) obj1 obj2 =
    do
        b <- checkRel grzH (GrzRel rel) obj1 obj2
        if b
            then
                return ()
            else do
                ptime <- getPOSIXTime
                let time = floor ptime
                h <- getStringHandle grzH rel
                grzRunSql grzH query $ map toSql [getID obj1,getID obj2,h,time]
                grzCommit grzH
             
    where
        query = "INSERT INTO relationships(guid1,guid2,relationshipType,timeCreated) VALUES(?,?,?,?)"
        
-- | Removes the relationship from the given objects (if it exists).        
delRel :: (GrzObjClass o1, GrzObjClass o2) => 
    GrzHandle   -- ^ data handle
    -> GrzRel   -- ^ relationship 
    -> o1       -- ^ first object
    -> o2       -- ^ second object 
    -> IO ()
delRel grzH (GrzRel rel) obj1 obj2 =
    do
        maybeH <- maybeGetStringHandle grzH rel
        case maybeH of
            Nothing -> return ()
            Just h -> do
                grzQuery grzH query $ map toSql [getID obj1, getID obj2, snd h]
                grzCommit grzH
    where
        query = "DELETE FROM relationships WHERE guid1 = ? AND guid2 = ? AND relationshipType = ?"   

-- | Returns True if the given relationship exists, otherwise False.   
checkRel :: (GrzObjClass o1, GrzObjClass o2) => 
    GrzHandle   -- ^ data handle
    -> GrzRel   -- ^ relationship
    -> o1       -- ^ first object
    -> o2       -- ^ second object
    -> IO Bool  -- ^ True or False
checkRel grzH (GrzRel rel) obj1 obj2 =
    do
        maybeH <- maybeGetStringHandle grzH rel
        case maybeH of
            Nothing -> return False
            Just h -> do            
                val <- grzQuery grzH query $ map toSql [getID obj1,getID obj2, snd h]
                return $ not $ null val
    where
        query = "SELECT * FROM relationships WHERE guid1 = ? AND guid2 = ? AND relationshipType = ?" 
        
-- | The relationship between an object and its container
hasContainer :: GrzRel
hasContainer = GrzRel "hasContainer"

-- | The relationship between an object and its owner
hasOwner :: GrzRel
hasOwner = GrzRel "hasOwner"

-- | The relationship between an object and its site
hasSite :: GrzRel
hasSite = GrzRel "hasSite"
        
{-|
  Tells the object store which fields are searchable.
-}        
setSearchable :: GrzObjClass o => 
    GrzHandle           -- ^ data handle
    -> (GrzObj -> o)    -- ^ type wrapper 
    -> [String]         -- ^ list of names for metadata to be searchable for this type wrapper
    -> IO ()
setSearchable grzH w ns = do
    oti <- getStringHandle grzH ot
    nsi <- mapM (getStringHandle grzH) ns
    grzRunSql grzH deleteQuery [toSql oti]
    mapM_ (grzRunSql grzH insertQuery) (map (\x -> [toSql oti, toSql x]) nsi)
    
    where
        ot = objWrapperToString (w emptyObj)
        deleteQuery = "DELETE FROM searchable WHERE typeID = ?"
        insertQuery = "INSERT INTO searchable(typeID, nameID) values (?,?)"
                    
{-|
  Runs a query definition and retrieves a list of 
  unwrapped objects from the database. Provide a limit of 0 to get all objects
  for the query.
-}        
getUnwrappedObjs ::
    GrzHandle                           -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> [String]                         -- ^ required metadata
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> IO [GrzObj]                      -- ^ list of objects
getUnwrappedObjs grzH queryDefs needs orderBy limit offset =
    do
        query <- grzCreateQuery grzH ((withData needs) . (setOrderBy orderBy) . queryDefs)
        result <- runQuery grzH $ addToQuery limitBit query
        return $ queryResultToObjs result
    where
        limitBit = getLimitBit grzH offset limit     
          
{-|
  Runs a query definition and retrieves a list of objects with the
  given type from the database. Provide a limit of 0 to get all objects for
  the query.
-}        
getObjs :: GrzObjClass o => 
        GrzHandle                       -- ^ data handle
    -> (GrzObj -> o)                    -- ^ wrapper
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> [String]                         -- ^ required metadata
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> IO [o]                           -- ^ list of objects
getObjs grzH w queryDefs needs orderBy limit offset =
    do
        query <- grzCreateQuery grzH ((withData needs) . (setOrderBy orderBy).(hasType w) . queryDefs)
        result <- runQuery grzH $ addToQuery limitBit query 
        return $ map w (queryResultToObjs result)
    where
        limitBit = getLimitBit grzH offset limit
                
{-|
  Runs a query definiton and retrieves a list of object ids from the
  database. Provide a limit of 0 to get all object ids for the query.
-}        
getObjIDs :: GrzHandle                  -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> IO [Int]                         -- ^ list of object IDs
getObjIDs grzH queryDefs orderBy limit offset =
    do
        query <- grzCreateQuery grzH ((setOrderBy orderBy) . queryDefs . (setQueryType GrzQTID))
        result <- runQuery grzH $ addToQuery limitBit query
        return $ map (\x -> fromSql $ head x) (fst result)
    where
        limitBit = getLimitBit grzH offset limit
        
{-|
  Runs a query definition and retrieves a list of bare objects
  (w (GrzObjID id) ) from the database. Provide a limit of 0 to get all objects
  for the query.
-}        
getBareObjs :: GrzObjClass o =>
        GrzHandle                       -- ^ data handle
    -> (GrzObj -> o)                    -- ^ wrapper
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> IO [o]                           -- ^ list of objects
getBareObjs grzH w queryDefs orderBy limit offset = 
    fmap (map (w . GrzObjID) ) (getObjIDs grzH ((hasType w) . queryDefs) orderBy offset limit)

{-|
  Runs a query definition and retrieves a list of unwrapped
  bare objects (GrzObjID id) from the database. Provide a limit of 0 to get all
  objects for the query.
-}        
getUnwrappedBareObjs :: 
        GrzHandle                       -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> IO [GrzObj]                           -- ^ list of objects
getUnwrappedBareObjs grzH queryDefs orderBy limit offset = fmap (map GrzObjID) (getObjIDs grzH queryDefs orderBy offset limit)

        
{-|
  Runs a query definition and retrieves a count of objects
  from the database.
-}        
getObjCount :: 
    GrzHandle                           -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition 
    -> IO Int                           -- ^ count of objects
getObjCount grzH queryDefs =
    do
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTCount))
        result <- runQuery grzH query
        return $ queryResultToCount result
                        
{-|
  Takes a query definition and two types, and retrieves a list of objects with 
  an aggregated count associated with each.
-}        
getObjsAggByObjCount :: (GrzObjClass o1, GrzObjClass o2) =>
    GrzHandle                            -- ^ data handle
    -> (GrzObj -> o1)                    -- ^ wrapper for type to be aggregated
    -> (GrzObj -> o2)                    -- ^ wrapper for result type
    -> (GrzQueryDef -> GrzQueryDef)      -- ^ query definition
    -> [String]                          -- ^ required metadata
    -> [GrzOrderBy]                      -- ^ order by
    -> Int                               -- ^ limit (number of objects to return)
    -> Int                               -- ^ offset
    -> IO [(o2, Int)]                    -- ^ (object, count) pair
getObjsAggByObjCount grzH w1 w2 queryDefs needs orderBy limit offset =
    do
        t2 <- maybeGetStringHandle grzH (objWrapperToString (w2 emptyObj))
        case t2 of
            Nothing -> return []
            Just (_,i2) -> do
                query <- grzCreateQuery grzH ((withData needs) . (setAggOrderBy orderBy) . (hasType w1) . queryDefs . (setQueryType (GrzQTAggByObjCount i2)))
                result <- runQuery grzH $ addToQuery limitBit query 
                return $ queryResultToAggByObjCount w2 result
   where
        limitBit = getLimitBit grzH offset limit
        
{-|
  Takes a query definition, two types and a metadata 
  name; retrieves a list of objects with an aggregated sum 
  (of the metadata value) and the count associated with each.
-}        
getObjsAggByObjSumCount :: (GrzObjClass o1, GrzObjClass o2, GrzAtomKeyClass k) =>
    GrzHandle                            -- ^ data handle
    -> (GrzObj -> o1)                    -- ^ wrapper for type to be aggregated
    -> (GrzObj -> o2)                    -- ^ wrapper for result type
    -> (GrzQueryDef -> GrzQueryDef)      -- ^ query definition
    -> GrzAtomKey k                      -- ^ metadata name attached to values being aggregated
    -> [String]                          -- ^ required metadata
    -> [GrzOrderBy]                      -- ^ order by
    -> Int                               -- ^ limit (number of objects to return)
    -> Int                               -- ^ offset
    -> IO [(o2, (Int,Int))]              -- ^ (object, count) pair
getObjsAggByObjSumCount grzH w1 w2 queryDefs name needs orderBy limit offset =
    do
        t2 <- maybeGetStringHandle grzH (objWrapperToString (w2 emptyObj))
        case t2 of
            Nothing -> return []
            Just (_,i2) -> do
                mn <- maybeGetStringHandle grzH (atomKey name)
                case mn of
                    Nothing -> return []
                    Just (_,n) -> do
                        query <- grzCreateQuery grzH ((withData needs) . (setAggOrderBy orderBy) . (hasType w1) . queryDefs . (setQueryType (GrzQTAggByObjSumCount i2 n)))
                        result <- runQuery grzH $ addToQuery limitBit query 
                        return $ queryResultToAggByObjSumCount w2 result
   where
        limitBit = getLimitBit grzH offset limit
        
{-|
  Takes a query definition and a metadata name; 
  retrieves a tuple (sum, count) of objects from the database that have 
  metadata integer values with that name.
-}        
getObjSumCount :: GrzAtomKeyClass k =>
    GrzHandle                          -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)    -- ^ query definition
    -> GrzAtomKey k                    -- ^ metadata name 
    -> IO (Int, Int)                   -- ^ sum and count of objects
getObjSumCount grzH queryDefs name =
    do
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTAggSumCount) . (hasData name))
        result <- runQuery grzH query
        return $ queryResultToSumCount result
                               
-- utilities

trimWhiteSpace :: String -> String
trimWhiteSpace = dropWhile isSpace . reverse . dropWhile isSpace . reverse        
