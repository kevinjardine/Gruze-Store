module Database.Gruze.Connection.ODBC (

getHandle, getMySQLHandle, getPostgreSQLHandle

) where

import Database.Gruze

import Database.HDBC
import Database.HDBC.ODBC

-- gets the handle
getGenericHandle :: (GrzAtomBox -> GrzAtomBox)
    -> GrzDatabaseType
    -> IO GrzHandle   
getGenericHandle c db = do
    dbc <- getDatabaseConnection config
    return $ GrzHandle dbc dataDir convertLoc logFile defaultSite thumbDefs logLevel db        
    where
        config = c emptyAtomBox
        dataDir = getString "grzDataDirectory" "" config
        convertLoc = getString "grzConvertLocation" "" config
        logFile = getString "grzLogFile" "" config
        defaultSite = emptyBareObj
        thumbDefs = []
        logLevel = WarningLogLevel
        
getMySQLHandle c = getGenericHandle c GrzMySQLDB
getPostgreSQLHandle c = getGenericHandle c GrzPostgreSQLDB

getHandle = getMySQLHandle

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