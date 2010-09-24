module Data.Store.Gruze.Utility

where

import Data.Store.Gruze.Box
import Data.Store.Gruze.DBTypes
import Data.Store.Gruze.Types

import Database.HDBC
import Database.HDBC.ODBC
import Data.Time.Clock.POSIX
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Data.Maybe

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
        grzLog grzH DebugLogLevel $ "query: " ++ query ++ " values: " ++ (show values)
        handleSqlError $ quickQuery' (grzDatabaseHandle grzH) query values
        
grzCommit :: GrzHandle -> IO ()
grzCommit grzH = commit (grzDatabaseHandle grzH)
  
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