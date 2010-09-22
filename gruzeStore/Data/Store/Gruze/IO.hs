module Data.Store.Gruze.IO (
    GrzHandle(..), 
    
    grzLog, getHandle, grzCommit, setDefaultSite, setThumbDefs, setLogLevel,
    
    -- query functions
    getObjs, getUnwrappedObjs, getBareObjs, getUnwrappedBareObjs, getObjIDs,
    getObjCount, getObjAggCount, getObjAggSumCount, setSearchable,
    
    -- object IO
    createObj, saveObj, delObj, disableObj, enableObj,
    loadObj, maybeLoadObj, maybeLoadContainer, maybeLoadOwner, maybeLoadSite,
    
    -- object pretty printers    
    ppObj, ppObjFull,

    -- file handler
    createFileAtom, maybeGetFileMetadata, maybeGetFileContent, maybeGetFileThumb,
    
    -- relationship IO
    addRel, delRel, checkRel
) where

-- functions to manage objects, string handles and file handles

import Data.Store.Gruze.Box
import Data.Store.Gruze.Types
import Data.Store.Gruze.QueryDef
import Data.Store.Gruze.Handles
import Data.Store.Gruze.Query
import Data.Store.Gruze.Utility
import Data.Store.Gruze.DBTypes

import Database.HDBC
import Database.HDBC.ODBC
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

-- the Object IO functions

-- handle functions

-- gets the handle
getHandle :: (GrzAtomBox -> GrzAtomBox) -> IO GrzHandle   
getHandle c = do
    let config = c emptyAtomBox
    dbc <- getDatabaseConnection config
    return $ GrzHandle {
            grzDatabaseHandle = dbc,
            grzDataDirectory = getString "grzDataDirectory" "" config,
            grzConvertLocation = getString "grzConvertLocation" "" config,
            grzLogFile = getString "grzLogFile" "" config,
            grzDefaultSite = emptyBareObj,
            grzThumbDefs = [],
            grzLogLevel = WarningLogLevel
        }
        
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
        grzQuery grzH query $ (map toSql [atomToStorageInt a,guid,h,safeAtomToInt a]) ++ [toSql $ safeAtomToString a]
        grzInsertMetadata grzH guid h as
    where
        query = "INSERT INTO metadata (metadataType,objectGuid,nameId,integerValue,stringValue) values(?,?,?,?,?)" 
grzInsertMetadata _ _ _ [] = return 0

grzDeleteMetadata :: GrzHandle -> Int -> Int -> IO [[SqlValue]]
grzDeleteMetadata grzH guid h =
    do
        grzQuery grzH query $ (map toSql [guid,h])
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
        val <- grzQuery grzH query $ map toSql [h,ownerID,containerID,siteID,time,time,enabled]
        qs <- grzQuery grzH "SELECT LAST_INSERT_ID() AS id" []
        let guid = ((fromSql (head (head qs)))::Int)
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
    GrzHandle    -- ^ The data handle 
    -> o         -- ^ The object to refresh
    -> [String]  -- ^ The names of the metadata to load
    -> IO o      -- ^ The returned object
loadObj grzH obj needs =
    do
        objs <- getUnwrappedObjs grzH qd 0 1
        return $ case objs of
                    [] -> replaceObj obj emptyObj
                    a  -> replaceObj obj (head a)
    where
        qd = (withObjs [obj]) . (withData needs)
        
maybeLoadObj :: GrzObjClass o => 
    GrzHandle           -- ^ The data handle
    -> (GrzObj -> o)    -- ^ the wrapper
    -> GrzObj           -- ^ The unwrapped object to load
    -> [String]         -- ^ The names of the metadata to load
    -> IO (Maybe o)       -- ^ The returned object
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
    -> [GrzString] 
    -> IO (Maybe oc)
    
maybeLoadContainer grzH w obj needs = maybeLoadObj grzH w (getContainer obj) needs

maybeLoadOwner :: (GrzObjClass o, GrzOwnerClass oo) => 
    GrzHandle 
    -> (GrzObj -> oo) 
    -> o 
    -> [GrzString] 
    -> IO (Maybe oo)
    
maybeLoadOwner grzH w obj needs = maybeLoadObj grzH w (getOwner obj) needs

maybeLoadSite :: (GrzObjClass o, GrzSiteClass os) => 
    GrzHandle 
    -> (GrzObj -> os) 
    -> o 
    -> [GrzString] 
    -> IO (Maybe os)
    
maybeLoadSite grzH w obj needs = maybeLoadObj grzH w (getSite obj) needs

-- TODO: need some exception handling here   
saveObj :: GrzObjClass o => GrzHandle -> o -> IO o
saveObj grzH o =
    do 
        ptime <- getPOSIXTime
        let time = floor ptime
        val <- grzQuery grzH query $ map toSql [ownerID,containerID,siteID,time,guid]
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
        else
            return r

delObjByID :: GrzHandle -> Int -> IO Bool
delObjByID grzH 0 = return True
delObjByID grzH guid =
    do
        -- delete the metadata 
        grzQuery grzH metadata_query $ [toSql guid]
        
        -- delete the relationships
        grzQuery grzH relationship_query $ [toSql guid,toSql guid]
        
        -- delete the owned objects
        owned <- getObjIDs grzH (hasOwners [GrzObjID guid]) 0 0
        mapM_ (delObjByID grzH) owned
        
        -- delete the contained objects
        contained <- getObjIDs grzH (hasContainers [GrzObjID guid]) 0 0
        mapM_ (delObjByID grzH) contained
        
        -- delete the objects that have this object as a site
        sited <- getObjIDs grzH (hasSites [GrzObjID guid]) 0 0
        mapM_ (delObjByID grzH) sited
        
        -- delete the object from the object table
        grzQuery grzH object_query $ [toSql guid]
        
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
        owned <- getObjIDs grzH (hasOwners [GrzObjID guid]) 0 0
        mapM_ (setEnableObjByID grzH state) owned
        
        -- set the enable state on the contained objects
        contained <- getObjIDs grzH (hasContainers [GrzObjID guid]) 0 0
        mapM_ (setEnableObjByID grzH state) contained
        
        -- set the enable state on the objects that have this object as a site
        sited <- getObjIDs grzH (hasSites [GrzObjID guid]) 0 0
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
ppObj' obj = "object " 
                ++ (show $ getID obj) 
                ++ " [" ++ (getType obj) 
                ++ "]"

ppObjFull :: GrzObjClass o => o -> String
ppObjFull = ppObjFull' . unwrapObj

ppObjFull' :: GrzObj -> String
ppObjFull' (GrzObjID i) = "object " ++ (show i)
ppObjFull' obj = "\nobject " 
                ++ (show $ getID obj) 
                ++ " [" ++ (getType obj) 
                ++ "] {\n\n"
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

addRel :: (GrzObjClass o1, GrzObjClass o2) => GrzHandle -> String -> o1 -> o2 -> IO ()
addRel grzH rel obj1 obj2 =
    do
        b <- checkRel grzH rel obj1 obj2
        if b
            then
                return ()
            else do
                ptime <- getPOSIXTime
                let time = floor ptime
                h <- getStringHandle grzH rel
                grzQuery grzH query $ map toSql [getID obj1,getID obj2,h,time]
                grzCommit grzH
             
    where
        query = "INSERT INTO relationships(guid1,guid2,relationshipType,timeCreated) VALUES(?,?,?,?)"
        
delRel :: (GrzObjClass o1, GrzObjClass o2) => GrzHandle -> String -> o1 -> o2 -> IO ()
delRel grzH rel obj1 obj2 =
    do
        maybeH <- maybeGetStringHandle grzH rel
        case maybeH of
            Nothing -> return ()
            Just h -> do
                grzQuery grzH query $ map toSql [getID obj1, getID obj2, snd h]
                grzCommit grzH
    where
        query = "DELETE FROM relationships WHERE guid1 = ? AND guid2 = ? AND relationshipType = ?"   
   
checkRel :: (GrzObjClass o1, GrzObjClass o2) => GrzHandle -> String -> o1 -> o2 -> IO Bool
checkRel grzH rel obj1 obj2 =
    do
        maybeH <- maybeGetStringHandle grzH rel
        case maybeH of
            Nothing -> return False
            Just h -> do            
                val <- grzQuery grzH query $ map toSql [getID obj1,getID obj2, snd h]
                return $ not $ null val
    where
        query = "SELECT * FROM relationships WHERE guid1 = ? AND guid2 = ? AND relationshipType = ?" 
        
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
    grzQuery grzH deleteQuery [toSql oti]
    mapM_ (grzQuery grzH insertQuery) (map (\x -> [toSql oti, toSql x]) nsi)
    
    where
        ot = objWrapperToString (w emptyObj)
        deleteQuery = "DELETE FROM searchable WHERE typeID = ?"
        insertQuery = "INSERT INTO searchable(typeID, nameID) values (?,?)"
        
{-|
  getUnwrappedObjs runs a query definition and retrieves a list of 
  unwrapped objects from the database. Provide a limit of 0 to get all objects
  for the query.
-}        
getUnwrappedObjs :: GrzHandle           -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> Int                              -- ^ offset
    -> Int                              -- ^ limit (number of objects to return)
    -> IO [GrzObj]                      -- ^ list of objects
getUnwrappedObjs grzH queryDefs offset limit =
    do
        query <- grzCreateQuery grzH queryDefs
        result <- runQuery grzH $ (addToQuery (addToQuery query orderBit) limitBit) 
        return $ queryResultToObjs result
    where
        limitBit = if limit == 0 then "" else " LIMIT " ++ (show offset) ++ "," ++ (show limit)
        orderBit = " ORDER BY q1.guid DESC "      
          
{-|
  getObjs runs a query definition and retrieves a list of objects with the
  given type from the database. Provide a limit of 0 to get all objects for
  the query.
-}        
getObjs :: GrzObjClass o => 
        GrzHandle                       -- ^ data handle
    -> (GrzObj -> o)                    -- ^ wrapper
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> Int                              -- ^ offset
    -> Int                              -- ^ limit (number of objects to return)
    -> IO [o]                           -- ^ list of objects
getObjs grzH w queryDefs offset limit =
    do
        query <- grzCreateQuery grzH (queryDefs . (hasType w))
        result <- runQuery grzH $ (addToQuery (addToQuery query orderBit) limitBit) 
        return $ map w (queryResultToObjs result)
    where
        limitBit = if limit == 0 then "" else " LIMIT " ++ (show offset) ++ "," ++ (show limit)
        orderBit = " ORDER BY q1.guid DESC "
        
{-|
  getObjIDs runs a query definiton and retrieves a list of object ids from the
  database. Provide a limit of 0 to get all object ids for the query.
-}        
getObjIDs :: GrzHandle                  -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> Int                              -- ^ offset
    -> Int                              -- ^ limit (number of objects to return)
    -> IO [Int]                         -- ^ list of object IDs
getObjIDs grzH queryDefs offset limit =
    do
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTID))
        result <- runQuery grzH $ (addToQuery (addToQuery query orderBit) limitBit) 
        return $ map (\x -> fromSql $ head x) (fst result)
    where
        limitBit = if limit == 0 then "" else " LIMIT " ++ (show offset) ++ "," ++ (show limit)
        orderBit = " ORDER BY guid DESC "
        
{-|
  getBareObjs runs a query definition and retrieves a list of bare objects
  (w (GrzObjID id) ) from the database. Provide a limit of 0 to get all objects
  for the query.
-}        
getBareObjs :: GrzObjClass o =>
        GrzHandle                       -- ^ The data handle
    -> (GrzObj -> o)                    -- ^ wrapper
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition
    -> Int                              -- ^ offset
    -> Int                              -- ^ limit (number of objects to return)
    -> IO [o]                           -- ^ list of objects
getBareObjs grzH w queryDefs offset limit = 
    fmap (map (w . GrzObjID) ) (getObjIDs grzH (queryDefs . (hasType w)) offset limit)

{-|
  getUnwrappedBareObjs runs a query definition and retrieves a list of unwrapped
  bare objects (GrzObjID id) from the database. Provide a limit of 0 to get all
  objects for the query.
-}        
getUnwrappedBareObjs :: 
        GrzHandle                       -- ^ The data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition
    -> Int                              -- ^ offset
    -> Int                              -- ^ limit (number of objects to return)
    -> IO [GrzObj]                           -- ^ list of objects
getUnwrappedBareObjs grzH queryDefs offset limit = fmap (map GrzObjID) (getObjIDs grzH queryDefs offset limit)

        
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
  The 'getObjAggCount' function takes a query definition and a metadata name.
  It retrieves a count of objects from the database that have metadata with that name.
-}        
getObjAggCount :: GrzHandle             -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> GrzString                        -- ^ metadata name 
    -> IO Int                           -- ^ count of objects
getObjAggCount grzH queryDefs name =
    do
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTAggCount) . (hasData name))
        result <- runQuery grzH query
        return $ queryResultToCount result
        
{-|
  The 'getObjAggSumCount' function takes a query definition and a metadata name.
  It retrieves a tuple (sum, count) of objects from the database that have 
  metadata integer values with that name.
-}        
getObjAggSumCount :: GrzHandle         -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)    -- ^ query definition
    -> GrzString                       -- ^ metadata name 
    -> IO (Int, Int)                   -- ^ count of objects
getObjAggSumCount grzH queryDefs name =
    do
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTAggSumCount) . (hasData name))
        result <- runQuery grzH query
        return $ queryResultToSumCount result
                
-- utilities

trimWhiteSpace :: String -> String
trimWhiteSpace = dropWhile isSpace . reverse . dropWhile isSpace . reverse          
