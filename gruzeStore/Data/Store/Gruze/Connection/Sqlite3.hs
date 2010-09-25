module Data.Store.Gruze.Connection.Sqlite3 (

Connection, getDatabaseConnection

) where

import Data.Store.Gruze.Box

import Database.HDBC
import Database.HDBC.Sqlite3

-- Sqlite3 interface            
getDatabaseConnection :: GrzAtomBox -> IO Connection
getDatabaseConnection config = do
    dbc <- connectSqlite3 (getString "grzDBFile" "" config)
    return dbc