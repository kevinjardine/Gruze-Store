module Database.Gruze.Connection.PostgreSQL (

getHandle

) where

import Database.Gruze

import Database.HDBC
import Database.HDBC.PostgreSQL

-- gets the handle
getHandle :: (GrzAtomBox -> GrzAtomBox)
    -> IO GrzHandle   
getHandle c = do
    dbc <- getDatabaseConnection config
    return $ GrzHandle dbc dataDir convertLoc logFile defaultSite thumbDefs logLevel GrzPostgreSQLDB        
    where
        config = c emptyAtomBox
        dataDir = getString "grzDataDirectory" "" config
        convertLoc = getString "grzConvertLocation" "" config
        logFile = getString "grzLogFile" "" config
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
            ++ (getString "grzDBUID" "" config)
            ++ "' password='"
            ++ (getString "grzDBPassword" "" config)
            ++ "' "
            ++ "port=" 
            ++ (getString "grzDBPort" "" config)
            ++ " host="
            ++ (getString "grzDBServer" "" config)
            ++ " dbname='"
            ++ (getString "grzDBDatabase" "" config)
            ++ "'"
    