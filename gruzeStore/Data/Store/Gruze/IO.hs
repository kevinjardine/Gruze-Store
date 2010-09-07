module Data.Store.Gruze.IO (
    GrzHandle(..), 
    
    grzLog, getHandler, grzCommit,
    
    -- query functions
    getObjs, getBareObjs, getObjIDs, getObjCount, getObjAggCount, getObjAggSumCount,
    setSearchable,
    
    -- object IO
    createObj, loadObj, saveObj, delObj,
    
    -- object pretty printers    
    ppObj, ppObjFull,

    -- file handler
    createFileAtom, maybeGetFileMetadata, maybeGetFileContent, maybeGetFileThumb,
    
    -- relationship IO
    addRel, delRel, checkRel
) where

-- functions to manage objects, string handles and file handles

import Data.Store.Gruze.Container
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

-- gets the handle

getHandler :: (GrzAtomBox -> GrzAtomBox) -> IO GrzHandle   
getHandler c = do
    let config = c emptyAtomBox
    dbc <- getDatabaseConnection config
    return $ GrzHandle {
            grzDatabaseHandle = dbc,
            grzDataDirectory = getString "grzDataDirectory" "" config,
            grzConvertLocation = getString "grzConvertLocation" "" config,
            grzLogFile = getString "grzLogFile" "" config,
            grzDefaultSite = invalidObj
        }

        
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
        -- grzLog $ "In grzSetMetadataArray, resetting array: " ++ (show (d:ds)) ++ "\n"
        h <- getStringHandle grzH (fst d)
        grzDeleteMetadata grzH guid h
        grzAddMetadata grzH guid h d
        grzSetMetadataArray grzH guid ds
grzSetMetadataArray _ _ [] = return 0

-- higher level object functions

createObj :: GrzHandle -> (GrzObj -> GrzObj) -> IO GrzObj
createObj grzH p =
    if null t then 
        return invalidObj
    else do
        ptime <- getPOSIXTime
        let time = floor ptime
        h <- getStringHandle grzH t
        val <- grzQuery grzH query $ map toSql [h,ownerID,containerID,siteID,time,time,enabled]
        qs <- grzQuery grzH "SELECT LAST_INSERT_ID() AS id" []
        let guid = ((fromSql (head (head qs)))::Int)
        let theObj = obj {
            objID = guid, 
            objTimeCreated = time, 
            objTimeUpdated = time,
            objOwner = shrinkObj $ getOwner obj,
            objContainer = shrinkObj $ getContainer obj,
            objSite = shrinkObj $ getSite obj
        }
        grzLog grzH $ "in createObj, created the object " ++ (show theObj) ++ "\n"

        -- TODO: perhaps f could return an error condition triggering a rollback?
        f <- grzAddMetadataArray grzH guid (Map.toList $ objMetadata obj)
        grzCommit grzH
        return theObj
    where
        obj = p emptyObj
        enabled = if isEnabled obj then 1 else 0
        siteID = getID $ getSite obj
        ownerID = getID $ getOwner obj
        containerID = getID $ getContainer obj
        t = trimWhiteSpace $ getType obj 
        query = "INSERT INTO objects(objectType,ownerGuid,containerGuid,siteGuid,timeCreated,timeUpdated,enabled) values(?,?,?,?,?,?,?)"         

{-|
  The 'loadObj' function loads the current data for an object from the database,
  including the specified metadata.
-}        
loadObj :: GrzHandle    -- ^ The data handle 
    -> GrzObj           -- ^ The object to refresh
    -> [String]         -- ^ The names of the metadata to load
    -> IO GrzObj        -- ^ The returned object
loadObj grzH obj needs =
    do
        objs <- getObjs grzH qd 0 1
        return $ case objs of
                    [] -> invalidObj
                    a  -> head a
    where
        qd = (withObjs [obj]) . (withData needs)

-- TODO: need some exception handling here        
saveObj :: GrzHandle -> GrzObj -> IO GrzObj
saveObj grzH obj =
    do 
        ptime <- getPOSIXTime
        let time = floor ptime
        val <- grzQuery grzH query $ map toSql [ownerID,containerID,siteID,time,guid]
        let theObj = obj {objTimeUpdated = time}
        -- TODO: perhaps f could return an error condition triggering a rollback?
        f <- grzSetMetadataArray grzH guid (Map.toList $ getMetadata theObj)
        grzCommit grzH
        return theObj
    where
        guid = getID obj
        enabled = if isEnabled obj then 1 else 0
        siteID = getID $ getSite obj
        ownerID = getID $ getOwner obj
        containerID = getID $ getContainer obj
        query = "UPDATE objects set ownerGuid = ?, containerGuid = ?, siteGuid = ?, timeUpdated = ? WHERE guid = ?"

{-|
  delObj deletes all the object data including metadata and relationships and then recursively
  deletes all the objects that have this object as a container, owner or site.
  
  TODO: add a version that allows providing a function to do something before an event is deleted.
-} 

delObj :: GrzHandle -> GrzObj -> IO Bool
delObj grzH obj = delObjByID grzH (getID obj)

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
        
        -- all done, so commit the result
        -- TODO: this will result in deleting some but not all related
        -- objects in case of a failure
        -- should perhaps the commit be at a higher level
        -- so that delete is all or none?
        grzCommit grzH
        
        return True
    where
        metadata_query = "DELETE FROM metadata WHERE objectGuid = ?"
        relationship_query = "DELETE FROM relationships WHERE guid1 = ? OR guid2 = ?"
        object_query = "DELETE FROM objects WHERE guid = ?"
        
ppObj :: GrzObj -> String
ppObj (GrzObjID i) = "object " ++ (show i)
ppObj obj = "object " 
                ++ (show $ getID obj) 
                ++ " [" ++ (getType obj) 
                ++ "]"

ppObjFull :: GrzObj -> String
ppObjFull (GrzObjID i) = "object " ++ (show i)
ppObjFull obj = "\nobject " 
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

addRel :: GrzHandle -> String -> GrzObj -> GrzObj -> IO ()
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
        
delRel :: GrzHandle -> String -> GrzObj -> GrzObj -> IO ()
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
   
checkRel :: GrzHandle -> String -> GrzObj -> GrzObj -> IO Bool
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
        
setSearchable :: GrzHandle -> String -> [String] -> IO ()
setSearchable grzH ot ns = do
    oti <- getStringHandle grzH ot
    nsi <- mapM (getStringHandle grzH) ns
    grzQuery grzH deleteQuery [toSql oti]
    mapM_ (grzQuery grzH insertQuery) (map (\x -> [toSql oti, toSql x]) nsi)
    
    where
        deleteQuery = "DELETE FROM searchable WHERE typeID = ?"
        insertQuery = "INSERT INTO searchable(typeID, nameID) values (?,?)"      
          
{-|
  The 'getObjs' function runs a query definiton and retrieves a list of objects
  from the database. Provide a limit of 0 to get all objects for the query.
-}        
getObjs :: GrzHandle                 -- ^ The data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition
    -> Int                              -- ^ offset
    -> Int                              -- ^ limit (number of objects to return)
    -> IO [GrzObj]                      -- ^ list of objects
getObjs grzH queryDefs offset limit =
    do
        query <- grzCreateQuery grzH queryDefs
        -- grzLog $ (snd query) ++ "\n"
        result <- runQuery grzH $ (addToQuery (addToQuery query orderBit) limitBit) 
        -- grzLog $ (show result) ++ "\n"
        return $ queryResultToObjs result
    where
        limitBit = if limit == 0 then "" else " LIMIT " ++ (show offset) ++ "," ++ (show limit)
        orderBit = " ORDER BY q1.guid DESC "
        
{-|
  The 'getObjIDs' function runs a query definiton and retrieves a list of object ids
  from the database. Provide a limit of 0 to get all object ids for the query.
-}        
getObjIDs :: GrzHandle               -- ^ The data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition
    -> Int                              -- ^ offset
    -> Int                              -- ^ limit (number of objects to return)
    -> IO [Int]                         -- ^ list of object IDs
getObjIDs grzH queryDefs offset limit =
    do
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTID))
        -- grzLog $ (snd query) ++ "\n"
        result <- runQuery grzH $ (addToQuery (addToQuery query orderBit) limitBit) 
        -- grzLog $ (show result) ++ "\n"
        return $ map (\x -> fromSql $ head x) (fst result)
    where
        limitBit = if limit == 0 then "" else " LIMIT " ++ (show offset) ++ "," ++ (show limit)
        orderBit = " ORDER BY guid DESC "
        
{-|
  The 'getBareObjs' function runs a query definiton and retrieves a list of bare objects
  (GrzObjID id) from the database. Provide a limit of 0 to get all objects for the query.
-}        
getBareObjs :: GrzHandle               -- ^ The data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition
    -> Int                              -- ^ offset
    -> Int                              -- ^ limit (number of objects to return)
    -> IO [GrzObj]                         -- ^ list of objects
getBareObjs grzH queryDefs offset limit = fmap (map GrzObjID) (getObjIDs grzH queryDefs offset limit)
        
{-|
  The 'getObjCount' function runs a query definition and retrieves a count of objects
  from the database.
-}        
getObjCount :: GrzHandle -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition 
    -> IO Int         -- ^ count of objects
getObjCount grzH queryDefs =
    do
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTCount))
        -- grzLog $ (snd query) ++ "\n"
        result <- runQuery grzH query
        -- grzLog $ (show result) ++ "\n"
        return $ queryResultToCount result
        
{-|
  The 'getObjAggCount' function takes a query definition and a metadata name.
  It retrieves a count of objects from the database that have metadata with that name.
-}        
getObjAggCount :: GrzHandle         -- ^ data handle
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> GrzString                        -- ^ metadata name 
    -> IO Int                           -- ^ count of objects
getObjAggCount grzH queryDefs name =
    do
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTAggCount) . (hasAgg name))
        -- grzLog $ (snd query) ++ "\n"
        result <- runQuery grzH query
        -- grzLog $ (show result) ++ "\n"
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
        query <- grzCreateQuery grzH (queryDefs . (setQueryType GrzQTAggSumCount) . (hasAgg name))
        -- grzLog $ (snd query) ++ "\n"
        result <- runQuery grzH query
        -- grzLog $ (show result) ++ "\n"
        return $ queryResultToSumCount result
         
-- utilities

trimWhiteSpace :: String -> String
trimWhiteSpace = dropWhile isSpace . reverse . dropWhile isSpace . reverse          
