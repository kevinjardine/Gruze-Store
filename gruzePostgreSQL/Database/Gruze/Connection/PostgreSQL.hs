module Database.Gruze.Connection.PostgreSQL (

getHandle, dataDirectory, logFile, convertLocation, dBServer, dBDatabase, 
dBUID, dBPassword, dBPort

) where

import Database.Gruze

import Database.HDBC
import Database.HDBC.PostgreSQL

dataDirectory = GrzAtomKey { atomKey = "dataDirectory" } :: GrzAtomStringKey
logFile = GrzAtomKey { atomKey = "logFile" } :: GrzAtomStringKey
convertLocation = GrzAtomKey { atomKey = "convertLocation" } :: GrzAtomStringKey
dBServer = GrzAtomKey { atomKey = "dBServer" } :: GrzAtomStringKey
dBDatabase = GrzAtomKey { atomKey = "dBDatabase" } :: GrzAtomStringKey
dBUID = GrzAtomKey { atomKey = "dBUID" } :: GrzAtomStringKey
dBPassword = GrzAtomKey { atomKey = "dBPassword" } :: GrzAtomStringKey
dBPort = GrzAtomKey { atomKey = "dBPort" } :: GrzAtomStringKey

-- gets the handle
getHandle :: (GrzAtomBox -> GrzAtomBox)
    -> IO GrzHandle   
getHandle c = do
    dbc <- getDatabaseConnection config
    return $ GrzHandle dbc 
        (get dataDirectory "" config) 
        (get convertLocation "" config) 
        (get logFile "" config) 
        defaultSite thumbDefs logLevel GrzPostgreSQLDB        
    where
        config = c emptyAtomBox
        defaultSite = emptyBareObj
        thumbDefs = []
        logLevel = WarningLogLevel
        
-- PostgreSQL interface            
getDatabaseConnection :: GrzAtomBox -> IO Connection
getDatabaseConnection config = do
    putStrLn connectionString
    dbc <- connectPostgreSQL (connectionString)
    return dbc    
    where
        connectionString = "user='"
            ++ (get dBUID "" config)
            ++ "' password='"
            ++ (get dBPassword "" config)
            ++ "' "
            ++ "port=" 
            ++ (get dBPort "" config)
            ++ " host="
            ++ (get dBServer "" config)
            ++ " dbname='"
            ++ (get dBDatabase "" config)
            ++ "'"
    