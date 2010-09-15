module Data.Store.Gruze.Handles

where

import Data.Store.Gruze.Utility
import Data.Store.Gruze.Box
import Data.Store.Gruze.Types
import Data.Store.Gruze.DBTypes

import Database.HDBC
import Database.HDBC.ODBC
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe
import Data.HashTable (hashString)
import System.Random
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import System.Process
import System.Exit (ExitCode)
import System.Directory

-- string handler functions

-- currently all string handles are stored only in the database
-- the string handle system is only likely to pay off when there is
-- some kind of value cache like memcached so that common string handles
-- can be looked up in memory without accessing the database

-- tries to extract valid string handle from query result     
getStringHandleFromQueryResult :: String -> [SqlValue] -> Maybe Int
getStringHandleFromQueryResult s [sqlId, sqlString] = 
    case fromSql sqlString of
            Just s2 -> case s2 == s of
                        True -> Just ((fromSql sqlId)::Int)
                        False -> Nothing
            Nothing -> Nothing

-- gets existing string handle or creates one if it doesn't exist
getStringHandle :: GrzHandle -> String -> IO Int
getStringHandle grzH s =
    do
        h <- maybeGetStringHandle grzH s
        case h of
            Nothing -> do
                        let hash_s = fromIntegral (hashString s)
                        makeStringHandle grzH s hash_s
            Just i -> return $ snd i
        
-- returns Just existing (string, string handle) pair or Nothing        
maybeGetStringHandle :: GrzHandle -> String -> IO (Maybe (String, Int))
maybeGetStringHandle grzH s =
    do
        qs <- grzQuery grzH ("SELECT id, string FROM names WHERE hash = " ++ (show hash_s)) []
        let ids = catMaybes $ map (getStringHandleFromQueryResult s) qs
        if null ids 
            then do
                grzLog grzH NotificationLogLevel ("Unable to find handle for string: " ++ s)
                return Nothing 
            else 
                return $ Just (s, head ids)
    where
        hash_s = fromIntegral (hashString s)

-- returns a dictionary of string handles, or nothing 
maybeGetStringHandles :: GrzHandle -> [String] -> IO (Maybe [(String,Int)])    
maybeGetStringHandles grzH ss = do
    r <- mapM (maybeGetStringHandle grzH) ss
    if and $ map isJust r
        then
            return $ Just (map fromJust r)
        else
            return Nothing

-- makes a new string handle and inserts it in the database   
makeStringHandle :: GrzHandle -> String -> Int -> IO Int
makeStringHandle grzH s hash_s =
    do
        val <- grzQuery grzH query [toSql s, toSql hash_s]
        result <- grzQuery grzH "SELECT LAST_INSERT_ID() AS id" []
        return ((fromSql (head (head result)))::Int)
    where
        query = "INSERT INTO names(string,hash) values(?,?)"

-- functions to manage files

-- TODO: getOrphanFiles

getFileHandle :: GrzHandle -> String -> String -> String -> String -> Int -> IO Int
getFileHandle grzH ofn ct locd locf time = do
        val <- grzQuery grzH query [toSql ofn, toSql ct, toSql locd, toSql locf, toSql time]
        result <- grzQuery grzH "SELECT LAST_INSERT_ID() AS id" []
        return ((fromSql (head (head result)))::Int)
    where
        query = "INSERT INTO files(originalName,contentType,locationDir,locationFile,timeCreated) values(?,?,?,?,?)"
        
maybeGetFileMetadata :: GrzHandle -> GrzAtom -> IO (Maybe ([String],Int))
maybeGetFileMetadata grzH (GrzAtomFile i) = do
        val <- grzQuery grzH query [toSql i]
        return $ getFileMetadataResult val
    where
        query = "SELECT originalName,contentType,locationDir,locationFile,timeCreated FROM files WHERE id = ?"
        
maybeGetFileMetadata grzH _ = return Nothing

maybeGetFileContent' :: GrzHandle -> GrzAtom -> String -> IO (Maybe B.ByteString)
maybeGetFileContent' grzH a@(GrzAtomFile i) prefix = do
        md <- maybeGetFileMetadata grzH a
        case md of
                    Just ([_,_,locd,locf],_) -> do
                                                    v <- (BS.readFile (
                                                        (grzDataDirectory grzH) 
                                                        ++ "/" 
                                                        ++ locd
                                                        ++ "/" 
                                                        ++ prefix
                                                        ++ locf))
                                                    return $ Just v
                    otherwise -> return Nothing
maybeGetFileContent' _ _ _ = return Nothing
                    
maybeGetFileContent :: GrzHandle -> GrzAtom -> IO (Maybe B.ByteString)
maybeGetFileContent grzH a = maybeGetFileContent' grzH a ""

maybeGetFileThumb :: GrzHandle -> String -> GrzAtom -> IO (Maybe B.ByteString)
maybeGetFileThumb grzH s a = maybeGetFileContent' grzH a s

delFile :: GrzHandle -> GrzAtom -> IO Bool
delFile grzH a@(GrzAtomFile i) = do
        fd <- maybeGetFileMetadata grzH a
        case fd of
            Just ([_,_,locd,_],_) -> do 
                                        removeDirectoryRecursive (dataDir ++ "/" ++ locd)
                                        grzQuery grzH queryMetadata [toSql i]
                                        grzQuery grzH queryFiles [toSql i]
                                        grzCommit grzH
                                        return True
            otherwise -> return False  
    where
        queryFiles = "DELETE FROM files WHERE id = ?"
        queryMetadata = "DELETE FROM metadata WHERE metadataType = 3 AND integerValue = ?"
        dataDir = grzDataDirectory grzH
        
delFile _ _ = return False

getFileMetadataResult :: [[SqlValue]] -> Maybe ([String],Int)
getFileMetadataResult [[SqlNull,_,_,_]] = Nothing
getFileMetadataResult [[ofn, ct, locd, locf, time]] = Just $ (map fromSql [ofn, ct, locd, locf], fromSql time)

-- TODO: fix this function to make sure it cannot generate
-- a name that already exists
generateFileLocationDirectory dataDir d = do
    rn <- getStdRandom (randomR (0::Int,5000))
    let loc = "uploads/" ++ d ++ "/" ++ (show rn)
    let dir = dataDir ++ "/" ++ loc
    createDirectoryIfMissing True dir
    return loc
      
createFileAtom :: GrzHandle -> String -> String -> B.ByteString -> IO GrzAtom
createFileAtom grzH ofn ct ubs =  do
        ptime <- getPOSIXTime
        let time = floor ptime
        loc <- saveFile grzH ofn ct ubs time
        h <- getFileHandle grzH ofn ct (fst loc) (snd loc) time
        return $ GrzAtomFile h
    
saveFile :: GrzHandle -> String -> String -> B.ByteString -> Int -> IO (String, String)
saveFile grzH ofn ct ubs time = do
    let dataDir = grzDataDirectory grzH    
    locd <- generateFileLocationDirectory dataDir (show time)
    let ffn = dataDir ++ "/" ++ locd ++ "/" ++ ofn
       
    BS.writeFile ffn ubs
    
    -- create thumbnails if any defined
    if (ct `elem` ["image/jpeg","image/jpg","image/gif","image/png","image/pjpeg","image/x-png"]) 
        then do
            mapM_ (resizeImage grzH (dataDir ++ "/" ++ locd) ofn) (grzThumbDefs grzH)
            -- TODO: check the exit codes?
            return ()
        else return ()
    return (locd, ofn)
    
resizeImage :: GrzHandle -> FilePath -> String -> (String,String) -> IO ExitCode
resizeImage grzH dir ofn (tn,ts) = do
    -- use rawSystem to run an imagemagick command
    let gcl = grzConvertLocation grzH
    let args = [dir ++ "/" ++ ofn,
                "-scale", ts,
                dir ++ "/" ++ tn ++ ofn
               ]
    grzLog grzH DebugLogLevel (show (gcl,args))
    code <- rawSystem gcl args
    grzLog grzH DebugLogLevel $ "Exit code: " ++ (show code)
    return code