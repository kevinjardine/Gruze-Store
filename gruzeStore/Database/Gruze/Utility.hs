module Database.Gruze.Utility

where

import Database.Gruze.Box
import Database.Gruze.Types

import Database.HDBC
import Data.Time.Clock.POSIX
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Data.Maybe

-- the slightly strange record handling here is due to the fact that the first
-- value of the GrzHandle record is a polymorphic type and apparently standard
-- record accessor functions cannot be used to store it. Moreover only
-- pattern matching can be used to extract the value. Hence the several
-- grzH@( GrzHandle {grzDatabaseHandle = dbc} ) references below.
   
getLastInsertId grzH = do
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
grzQuery grzH@( GrzHandle {grzDatabaseHandle = dbc} ) query values =
    do
        grzLog grzH DebugLogLevel $ "query: " ++ query ++ " values: " ++ (show values)
        handleSqlError $ quickQuery' dbc query values
        
grzRunSql :: GrzHandle -> String -> [SqlValue] -> IO ()  
grzRunSql grzH@( GrzHandle {grzDatabaseHandle = dbc} ) query values =
    do
        grzLog grzH DebugLogLevel $ "run sql: " ++ query ++ " values: " ++ (show values)
        handleSqlError $ run dbc query values
        return ()
        
grzCommit :: GrzHandle -> IO ()
grzCommit grzH@( GrzHandle {grzDatabaseHandle = dbc} ) = commit dbc

grzRollback :: GrzHandle -> IO ()
grzRollback grzH@( GrzHandle {grzDatabaseHandle = dbc} ) = rollback dbc
  
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
 