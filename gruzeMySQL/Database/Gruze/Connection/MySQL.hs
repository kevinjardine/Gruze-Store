module Database.Gruze.Connection.MySQL (

getHandle, dataDirectory, logFile, convertLocation, dBServer, dBDatabase, 
dBUID, dBPassword, dBSocket

) where

import Database.Gruze

import Database.HDBC
import Database.HDBC.MySQL

dataDirectory = GrzAtomKey { atomKey = "dataDirectory" } :: GrzAtomStringKey
logFile = GrzAtomKey { atomKey = "logFile" } :: GrzAtomStringKey
convertLocation = GrzAtomKey { atomKey = "convertLocation" } :: GrzAtomStringKey
dBServer = GrzAtomKey { atomKey = "dBServer" } :: GrzAtomStringKey
dBDatabase = GrzAtomKey { atomKey = "dBDatabase" } :: GrzAtomStringKey
dBUID = GrzAtomKey { atomKey = "dBUID" } :: GrzAtomStringKey
dBPassword = GrzAtomKey { atomKey = "dBPassword" } :: GrzAtomStringKey
dBSocket = GrzAtomKey { atomKey = "dBSocket" } :: GrzAtomStringKey

-- gets the handle
getHandle :: (GrzAtomBox -> GrzAtomBox)
    -> IO GrzHandle   
getHandle c = do
    dbc <- getDatabaseConnection config
    return $ GrzHandle dbc 
        (get dataDirectory "" config) 
        (get convertLocation "" config) 
        (get logFile "" config) 
        defaultSite thumbDefs logLevel GrzMySQLDB        
    where
        config = c emptyAtomBox
        defaultSite = emptyBareObj
        thumbDefs = []
        logLevel = WarningLogLevel
        
-- MySQL interface            
getDatabaseConnection :: GrzAtomBox -> IO Connection
getDatabaseConnection config = do
    dbc <- connectMySQL defaultMySQLConnectInfo {
                        mysqlHost = get dBServer "" config,
                        mysqlDatabase = get dBDatabase "" config,
                        mysqlUser = get dBUID "" config,
                        mysqlPassword = get dBPassword "" config,
                        mysqlUnixSocket = get dBSocket "" config
            }
    return dbc
