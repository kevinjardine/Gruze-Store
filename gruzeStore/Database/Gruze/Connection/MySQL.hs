module Data.Store.Gruze.Connection.MySQL (

Connection, getDatabaseConnection

) where

import Data.Store.Gruze.Box

import Database.HDBC
import Database.HDBC.MySQL

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
