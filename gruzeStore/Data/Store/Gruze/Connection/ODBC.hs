module Data.Store.Gruze.Connection.ODBC (

Connection, getDatabaseConnection

) where

import Data.Store.Gruze.Box

import Database.HDBC
import Database.HDBC.ODBC

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