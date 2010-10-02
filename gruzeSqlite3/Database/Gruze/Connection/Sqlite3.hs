module Database.Gruze.Connection.Sqlite3 (

getHandle, dBFile, dataDirectory, logFile, convertLocation

) where

import Database.Gruze

import Database.HDBC
import Database.HDBC.Sqlite3

dBFile = GrzAtomKey { atomKey = "dBFilee" } :: GrzAtomStringKey
dataDirectory = GrzAtomKey { atomKey = "dataDirectory" } :: GrzAtomStringKey
logFile = GrzAtomKey { atomKey = "logFile" } :: GrzAtomStringKey
convertLocation = GrzAtomKey { atomKey = "convertLocation" } :: GrzAtomStringKey

-- gets the handle
getHandle :: (GrzAtomBox -> GrzAtomBox) 
    -> IO GrzHandle   
getHandle c = do
    dbc <- getDatabaseConnection config
    return $ GrzHandle dbc 
        (get dataDirectory "" config) 
        (get convertLocation "" config) 
        (get logFile "" config) 
        defaultSite thumbDefs logLevel GrzSqlite3DB        
    where
        config = c emptyAtomBox
        defaultSite = emptyBareObj
        thumbDefs = []
        logLevel = WarningLogLevel

-- Sqlite3 interface            
getDatabaseConnection :: GrzAtomBox -> IO Connection
getDatabaseConnection config = do
    dbc <- connectSqlite3 (get dBFile "" config)
    return dbc
    