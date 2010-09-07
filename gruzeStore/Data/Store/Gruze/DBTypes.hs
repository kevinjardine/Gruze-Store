module Data.Store.Gruze.DBTypes where

import Data.Store.Gruze.Types

import Database.HDBC
import Database.HDBC.ODBC

data GrzHandle = GrzHandle {
    grzDatabaseHandle :: Connection,
    grzDataDirectory :: FilePath,
    grzConvertLocation :: FilePath,
    grzLogFile :: FilePath,
    grzDefaultSite :: GrzObj
}