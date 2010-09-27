module Database.Gruze.Connection.Sqlite3 (

getHandle

) where

import Database.Gruze

import Database.HDBC
import Database.HDBC.Sqlite3

-- gets the handle
getHandle :: (GrzAtomBox -> GrzAtomBox) 
    -> IO GrzHandle   
getHandle c = do
    dbc <- getDatabaseConnection config
    return $ GrzHandle dbc dataDir convertLoc logFile defaultSite thumbDefs logLevel GrzSqlite3DB        
    where
        config = c emptyAtomBox
        dataDir = getString "grzDataDirectory" "" config
        convertLoc = getString "grzConvertLocation" "" config
        logFile = getString "grzLogFile" "" config
        defaultSite = emptyBareObj
        thumbDefs = []
        logLevel = WarningLogLevel

-- Sqlite3 interface            
getDatabaseConnection :: GrzAtomBox -> IO Connection
getDatabaseConnection config = do
    dbc <- connectSqlite3 (getString "grzDBFile" "" config)
    return dbc
    