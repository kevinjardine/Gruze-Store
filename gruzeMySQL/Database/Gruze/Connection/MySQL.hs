module Database.Gruze.Connection.MySQL (

getHandle

) where

import Database.Gruze

import Database.HDBC
import Database.HDBC.MySQL

-- gets the handle
getHandle :: (GrzAtomBox -> GrzAtomBox)
    -> IO GrzHandle   
getHandle c = do
    dbc <- getDatabaseConnection config
    return $ GrzHandle dbc dataDir convertLoc logFile defaultSite thumbDefs logLevel GrzMySQLDB        
    where
        config = c emptyAtomBox
        dataDir = getString "grzDataDirectory" "" config
        convertLoc = getString "grzConvertLocation" "" config
        logFile = getString "grzLogFile" "" config
        defaultSite = emptyBareObj
        thumbDefs = []
        logLevel = WarningLogLevel
        
-- MySQL interface            
getDatabaseConnection :: GrzAtomBox -> IO Connection
getDatabaseConnection config = do
    dbc <- connectMySQL defaultMySQLConnectInfo {
                        mysqlHost = getString "grzDBServer" "" config,
                        mysqlDatabase = getString "grzDBDatabase" "" config,
                        mysqlUser = getString "grzDBUID" "" config,
                        mysqlPassword = getString "grzDBPassword" "" config,
                        mysqlUnixSocket = getString "grzDBSocket" "" config
            }
    return dbc
