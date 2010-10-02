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
    noMetadata, allMetadata,
    
    -- object pretty printers    
    ppObj, ppObjFull,

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

import System.Locale (defaultTimeLocale)
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
        
setDefaultSite :: GrzSiteClass os => os -> GrzHandle -> GrzHandle
setDefaultSite site grzH  =               
    grzH {grzDefaultSite = unwrapObj site}
    
setThumbDefs :: [(String,String)] -> GrzHandle -> GrzHandle
setThumbDefs td grzH =               
    grzH {grzThumbDefs = td}
    
setLogLevel :: GrzLogLevel -> GrzHandle -> GrzHandle
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

-- takes a type constructor and some setters
-- and saves the object to the datebase, returning the new wrapped object

createObj :: (Typeable o, GrzObjClass o) => GrzHandle -> (GrzObj -> o) -> (GrzObj -> GrzObj) -> IO o
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
  The 'loadObj' function loads the current data for an object from the database,
  including the specified metadata.
-}        
loadObj :: GrzObjClass o => 
    GrzHandle       -- ^ The data handle 
    -> o            -- ^ The object to refresh
    -> [String]     -- ^ The names of the metadata to load
    -> IO o         -- ^ The returned object
loadObj grzH obj needs =
    do
        objs <- getUnwrappedObjs grzH qd needs [] 1 0
        return $ case objs of
                    [] -> replaceObj obj emptyObj
                    a  -> replaceObj obj (head a)
    where
        qd = (withObjs [obj])
        
maybeLoadObj :: GrzObjClass o => 
    GrzHandle           -- ^ The data handle
    -> (GrzObj -> o)    -- ^ the wrapper
    -> GrzObj           -- ^ The unwrapped object to load
    -> [String]       -- ^ The names of the metadata to load
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

maybeLoadContainer :: (GrzObjClass o, GrzObjClass oc, GrzContainerClass oc) => 
    GrzHandle 
    -> (GrzObj -> oc) 
    -> o 
    -> [String] 
    -> IO (Maybe oc)
    
maybeLoadContainer grzH w obj needs = maybeLoadObj grzH w (getContainer obj) needs

maybeLoadOwner :: (GrzObjClass o, GrzOwnerClass oo) => 
    GrzHandle 
    -> (GrzObj -> oo) 
    -> o 
    -> [String] 
    -> IO (Maybe oo)
    
maybeLoadOwner grzH w obj needs = maybeLoadObj grzH w (getOwner obj) needs

maybeLoadSite :: (GrzObjClass o, GrzSiteClass os) => 
    GrzHandle 
    -> (GrzObj -> os) 
    -> o 
    -> [String]
    -> IO (Maybe os)
    
maybeLoadSite grzH w obj needs = maybeLoadObj grzH w (getSite obj) needs

-- TODO: need some exception handling here   
saveObj :: GrzObjClass o => GrzHandle -> o -> IO o
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
  delObj deletes all the object data including metadata and relationships and recursively
  deletes all the objects that have this object as a container, owner or site.
  
  TODO: add a version that allows providing a function to do something before an object is deleted.
-} 

delObj :: GrzObjClass o => GrzHandle -> o -> IO Bool
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
  disableObj disables the object and recursively disables all the objects that
  have this object as a container, owner or site.
  
  TODO: add a version that allows providing a function to do something before
  an object is disabled.
-}         
disableObj grzH obj = setEnableObj grzH False obj

{-|
  enableObj enables the object and recursively enables all the objects that
  have this object as a container, owner or site.
  
  TODO: add a version that allows providing a function to do something before
  an object is enabled.
-}    
enableObj grzH obj = setEnableObj grzH True obj
        
ppObj :: GrzObjClass o => o -> String
ppObj = ppObj' . unwrapObj

ppObj' :: GrzObj -> String
ppObj' (GrzObjID i) = "object " ++ (show i)
ppObj' obj = (getType obj)
                ++ " "
                ++ (show $ getID obj) 

ppObjFull :: GrzObjClass o => o -> String
ppObjFull = ppObjFull' . unwrapObj

ppObjFull' :: GrzObj -> String
ppObjFull' (GrzObjID i) = "object " ++ (show i)
ppObjFull' obj = (getType obj)
                ++ " "
                ++ (show $ getID obj) 
                ++ " {\n\n"
                ++ "    timeCreated: \t"
                ++ (convertTime $ getTimeCreated obj)
                ++ "\n    timeUpdated: \t"
                ++ (convertTime $ getTimeUpdated obj)
                ++ "\n    owner: \t\t"
                ++ (show $ getOwner obj)
                ++ "\n    container: \t\t"
                ++ (show $ getContainer obj)
                ++ "\n    site: \t\t"
                ++ (show $ getSite obj)
                ++ "\n    enabled: \t\t"
                ++ (if isEnabled obj then "True" else "False")
                ++ "\n\n    metadata:\n\n"
                ++ (ppAtomBox $ getMetadata obj)
                ++ "\n}\n"           
     where
        convertTime t = (formatTime defaultTimeLocale "%c" (posixSecondsToUTCTime $ fromIntegral $ t))
        
ppAtomBox :: GrzAtomBox -> String
ppAtomBox box =
    intercalate "\n\n" $ map ppAtomPair (Map.toList box)
    
ppAtomPair :: (GrzKey,[GrzAtom]) -> String
ppAtomPair (k,v) = "        " ++ k ++ ": \n            " ++ (ppAtomList v) 

ppAtomList v = intercalate ", " (map ppAtom v)      
        
-- relationship functions
-- TODO: make relationships full types and not just strings                    

addRel :: (GrzObjClass o1, GrzObjClass o2) => GrzHandle -> GrzRel -> o1 -> o2 -> IO ()
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
        
delRel :: (GrzObjClass o1, GrzObjClass o2) => GrzHandle -> GrzRel -> o1 -> o2 -> IO ()
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
   
checkRel :: (GrzObjClass o1, GrzObjClass o2) => GrzHandle -> GrzRel -> o1 -> o2 -> IO Bool
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
        
-- define the three special relationships
hasContainer = GrzRel "hasContainer"
hasOwner = GrzRel "hasOwner"
hasSite = GrzRel "hasSite"
        
{-|
  setSearchable tells the object store which fields are searchable.
-} 
        
setSearchable :: GrzObjClass o => 
    GrzHandle           -- ^ data handle
    -> (GrzObj -> o)    -- ^ wrapper 
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
    
noMetadata = []
allMetadata = ["*"]

-- needsToKeys (NeedsAll) = ["*"]
-- needsToKeys (NeedsString x) = map atomKey x
-- needsToKeys (NeedsInt x) = map atomKey x
-- needsToKeys (NeedsBool x) = map atomKey x   
-- needsToKeys (NeedsAtom x) = map atomKey x     

-- getOrderBit grzH query orderBy = do
--     orderList <- mapM (getOrderBy grzH (getHandleDict query)) orderBy'
--     let orderBit = (concatMap snd orderList) ++ " ORDER BY " ++ (intercalate "," $ map fst orderList)
--     return orderBit
--     where
--         orderBy' = if null orderBy then [GuidDesc] else orderBy  
                    
{-|
  getUnwrappedObjs runs a query definition and retrieves a list of 
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
  getObjs runs a query definition and retrieves a list of objects with the
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
  getObjIDs runs a query definiton and retrieves a list of object ids from the
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
  getBareObjs runs a query definition and retrieves a list of bare objects
  (w (GrzObjID id) ) from the database. Provide a limit of 0 to get all objects
  for the query.
-}        
getBareObjs :: GrzObjClass o =>
        GrzHandle                       -- ^ The data handle
    -> (GrzObj -> o)                    -- ^ wrapper
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> IO [o]                           -- ^ list of objects
getBareObjs grzH w queryDefs orderBy limit offset = 
    fmap (map (w . GrzObjID) ) (getObjIDs grzH ((hasType w) . queryDefs) orderBy offset limit)

{-|
  getUnwrappedBareObjs runs a query definition and retrieves a list of unwrapped
  bare objects (GrzObjID id) from the database. Provide a limit of 0 to get all
  objects for the query.
-}        
getUnwrappedBareObjs :: 
        GrzHandle                       -- ^ The data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> IO [GrzObj]                           -- ^ list of objects
getUnwrappedBareObjs grzH queryDefs orderBy limit offset = fmap (map GrzObjID) (getObjIDs grzH queryDefs orderBy offset limit)

        
{-|
  The 'getObjCount' function runs a query definition and retrieves a count of objects
  from the database.
-}        
getObjCount :: 
    GrzHandle                           -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition 
    -> IO Int                           -- ^ count of objects
getObjCount grzH queryDefs =
    do
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTCount))
        result <- runQuery grzH query
        return $ queryResultToCount result
                        
{-|
  The 'getObjsAggByObjCount' function takes a query definition and two types.
  It retrieves a list of objects with an aggregated count associated with each.
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
  The 'getObjsAggByObjSumCount' function takes a query definition, a metadata 
  name and two types. It retrieves a list of objects with an aggregated sum 
  (of the metadata value) and count associated with each.
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
  The 'getObjSumCount' function takes a query definition and a metadata name.
  It retrieves a tuple (sum, count) of objects from the database that have 
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
