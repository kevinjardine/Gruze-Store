module Data.Store.Gruze.Utility

where

import Data.Store.Gruze.Box
import Data.Store.Gruze.Types

-- uncomment one of the three Connection options below:

import Data.Store.Gruze.Connection.Sqlite3
-- import Data.Store.Gruze.Connection.ODBC
-- import Data.Store.Gruze.Connection.PostgreSQL


import Database.HDBC
import Data.Time.Clock.POSIX
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Data.Maybe

data GrzHandle = GrzHandle {
    grzDatabaseHandle :: Connection,
    grzDataDirectory :: FilePath,
    grzConvertLocation :: FilePath,
    grzLogFile :: FilePath,
    grzDefaultSite :: GrzObj,
    grzThumbDefs :: [(String,String)],
    grzLogLevel :: GrzLogLevel,
    grzDatabaseType :: GrzDatabaseType
}
   
-- gets the handle
getHandle :: (GrzAtomBox -> GrzAtomBox) -> GrzDatabaseType -> IO GrzHandle   
getHandle c dbt = do
    let config = c emptyAtomBox
    dbc <- getDatabaseConnection config
    return $ GrzHandle {
            grzDatabaseHandle = dbc,
            grzDataDirectory = getString "grzDataDirectory" "" config,
            grzConvertLocation = getString "grzConvertLocation" "" config,
            grzLogFile = getString "grzLogFile" "" config,
            grzDefaultSite = emptyBareObj,
            grzThumbDefs = [],
            grzLogLevel = WarningLogLevel,
            grzDatabaseType = dbt
        }
    
getLastInsertId grzH = do
    
    -- qs <- grzQuery grzH "SELECT LAST_INSERT_ID() AS id" []
    -- sqlite3 version
    qs <- grzQuery grzH ("SELECT " ++ fc ++ " AS id") []
    return ((fromSql (head (head qs)))::Int)
    where
        fc = case grzDatabaseType grzH of
                    GrzMySQLDB -> "LAST_INSERT_ID()"
                    GrzSqlite3DB -> "last_insert_rowid()"
                    GrzPostgreSQLDB -> "lastval()"

getLimitBit grzH offset limit =                    
    if limit == 0 
        then
            "" 
        else 
            case grzDatabaseType grzH of
                GrzPostgreSQLDB -> " OFFSET " ++ (show offset) ++ " LIMIT " ++ (show limit)
                otherwise -> " LIMIT " ++ (show offset) ++ "," ++ (show limit)

-- for now this just replaces LIKE with ILIKE for PostgreSQL                
transformStringClause grzH sc =
    case grzDatabaseType grzH of
        GrzPostgreSQLDB -> replace sc "LIKE" "ILIKE"
        otherwise -> sc

grzQuery :: GrzHandle -> String -> [SqlValue] -> IO [[SqlValue]]  
grzQuery grzH query values =
    do
        grzLog grzH DebugLogLevel $ "query: " ++ query ++ " values: " ++ (show values)
        handleSqlError $ quickQuery' (grzDatabaseHandle grzH) query values
        
grzCommit :: GrzHandle -> IO ()
grzCommit grzH = commit (grzDatabaseHandle grzH)

grzRollback :: GrzHandle -> IO ()
grzRollback grzH = rollback (grzDatabaseHandle grzH)
  
logLevelToString = [    (DebugLogLevel,"DEBUG"),
                        (NotificationLogLevel,"NOTIFICATION"),
                        (WarningLogLevel,"WARNING"),
                        (FatalLogLevel,"FATAL")
                   ]
grzLog :: GrzHandle -> GrzLogLevel -> String -> IO ()                 
grzLog grzH level s = do
    if level >= (grzLogLevel grzH)
        then do
            ptime <- getCurrentTime
            let ftime = formatTime defaultTimeLocale "%c" ptime
            appendFile (grzLogFile grzH) 
                (ftime 
                    ++ ": " 
                    ++ (fromMaybe "" $ lookup level logLevelToString)
                    ++ ": " 
                    ++ s 
                    ++ "\n")
        else
            return ()
            
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)
 