module Data.Store.Gruze.Handles

where

import Data.Store.Gruze.Utility
import Data.Store.Gruze.Container
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
import System.Process (system)
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
        if null ids then return Nothing else return $ Just (s, head ids)
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

-- TODO: getFileData GrzHandle GrzAtom -> Maybe (String, String, String)
-- ofn, ct, loc
-- getOrphanFiles
-- delFile

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
        
getFileMetadata grzH _ = return Nothing

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

maybeGetFileThumb :: GrzHandle -> GrzAtom -> IO (Maybe B.ByteString)
maybeGetFileThumb grzH a = maybeGetFileContent' grzH a "thumb"

delFile :: GrzHandle -> GrzAtom -> IO Bool
delFile grzH a@(GrzAtomFile i) = do
        fd <- getFileMetadata grzH a
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

-- a stub function for now
-- need to create a directory based on
-- uploads/yyyy/mm/dd/rrrr
-- where rrrr is a random number   
generateFileLocationDirectory = "uploads"
      
createFileAtom :: GrzHandle -> String -> String -> String -> IO GrzAtom
createFileAtom grzH ofn ct ubs =  do
        ptime <- getPOSIXTime
        let time = floor ptime
        loc <- saveFile grzH ofn ct ubs time
        h <- getFileHandle grzH ofn ct (fst loc) (snd loc) time
        return $ GrzAtomFile h
    
saveFile :: GrzHandle -> String -> String -> String -> Int -> IO (String, String)
saveFile grzH ofn ct ubs time = do
    let dataDir = grzDataDirectory grzH
    
    rn <- getStdRandom (randomR (0::Int,5000))
    let loc = generateFileLocationDirectory 
    let nfn = (show rn) 
                ++ "_" 
                ++ (show time) 
                ++ "_"
                ++ ofn
    let ffn = dataDir ++ "/" ++ loc ++ "/" ++ nfn
    let tfn = dataDir ++ "/" ++ loc ++ "/thumb_" ++ nfn
    BS.writeFile ffn (B.pack ubs)
    if (ct `elem` ["image/jpeg","image/jpg","image/gif","image/png"]) 
        then do
            c <- resizeImage grzH ffn tfn 64 64
            -- TODO: check the exit code
            return ()
        else return ()
    return (loc, nfn)
    
resizeImage :: GrzHandle -> FilePath -> FilePath -> Integer -> Integer -> IO ExitCode
resizeImage grzH fn nfn w h = do
    -- use system to run an imagemagick command
    let gcl = grzConvertLocation grzH
    -- wrap file names in quotes (because under Windows they can have spaces)
    let cmd = gcl ++ " \"" ++ fn ++ "\" -scale " ++ (show w) ++ "x" ++ (show h) ++ " \"" ++ nfn ++ "\""
    grzLog grzH cmd
    system cmd