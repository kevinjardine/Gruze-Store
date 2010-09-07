module Data.Store.Gruze.Utility

where

import Data.Store.Gruze.Container
import Data.Store.Gruze.DBTypes

import Database.HDBC
import Database.HDBC.ODBC
import Data.Time.Clock.POSIX
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import System.Locale (defaultTimeLocale)

-- ODBC interface            
getDatabaseConnection :: GrzAtomBox -> IO Connection
getDatabaseConnection config = do
    dbc <- connectODBC ("DRIVER=" 
            ++ (getString "grzDBDriver" "" config)
            ++ "; SERVER="
            ++ (getString "grzDBServer" "" config)
            ++ "; DATABASE="
            ++ (getString "grzDBDatabase" "" config)
            ++ "; UID="
            ++ (getString "grzDBUID" "" config)
            ++ "; PASSWORD="
            ++ (getString "grzDBPassword" "" config)
            ++ ";")
    return dbc   

grzQuery :: GrzHandle -> String -> [SqlValue] -> IO [[SqlValue]]  
grzQuery grzH query values =
    do
        grzLog grzH $ "query: " ++ query ++ " values: " ++ (show values)
        handleSqlError $ quickQuery' (grzDatabaseHandle grzH) query values
        
grzCommit :: GrzHandle -> IO ()
grzCommit grzH = commit (grzDatabaseHandle grzH)

-- TODO: consider having log levels (eg. debug, notify, warning, error)
grzLog :: GrzHandle -> String -> IO ()                 
grzLog grzH s = do
    ptime <- getCurrentTime
    let ftime = formatTime defaultTimeLocale "%c" ptime
    appendFile (grzLogFile grzH) (ftime ++ ": " ++ s ++ "\n") 