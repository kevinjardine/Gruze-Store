module Database.Gruze.Connection.ODBC (

getHandle, getMySQLHandle, getPostgreSQLHandle,
dataDirectory, logFile, convertLocation, dBDriver, dBServer, dBDatabase, 
dBUID, dBPassword

) where

import Database.Gruze

import Database.HDBC
import Database.HDBC.ODBC

dataDirectory = GrzAtomKey { atomKey = "dataDirectory" } :: GrzAtomStringKey
logFile = GrzAtomKey { atomKey = "logFile" } :: GrzAtomStringKey
convertLocation = GrzAtomKey { atomKey = "convertLocation" } :: GrzAtomStringKey
dBDriver = GrzAtomKey { atomKey = "dBDriver" } :: GrzAtomStringKey
dBServer = GrzAtomKey { atomKey = "dBServer" } :: GrzAtomStringKey
dBDatabase = GrzAtomKey { atomKey = "dBDatabase" } :: GrzAtomStringKey
dBUID = GrzAtomKey { atomKey = "dBUID" } :: GrzAtomStringKey
dBPassword = GrzAtomKey { atomKey = "dBPassword" } :: GrzAtomStringKey

-- gets the handle
getGenericHandle :: (GrzAtomBox -> GrzAtomBox)
    -> GrzDatabaseType
    -> IO GrzHandle   
getGenericHandle c db = do
    dbc <- getDatabaseConnection config
    return $ GrzHandle dbc 
        (get dataDirectory "" config) 
        (get convertLocation "" config) 
        (get logFile "" config) 
        defaultSite thumbDefs logLevel db        
    where
        config = c emptyAtomBox
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
            ++ (get dBDriver "" config)
            ++ "; SERVER="
            ++ (get dBServer "" config)
            ++ "; DATABASE="
            ++ (get dBDatabase "" config)
            ++ "; UID="
            ++ (get dBUID "" config)
            ++ "; PASSWORD="
            ++ (get dBPassword "" config)
            ++ ";")
    return dbc