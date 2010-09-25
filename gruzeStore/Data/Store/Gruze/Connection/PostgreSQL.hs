module Data.Store.Gruze.Connection.PostgreSQL (

Connection, getDatabaseConnection

) where

import Data.Store.Gruze.Box

import Database.HDBC
import Database.HDBC.PostgreSQL

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
    