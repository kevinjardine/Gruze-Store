{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Gruze.Templates (

defObj, defContainer, defOwner, defSite, defOwnerContainer

) where

import Database.Gruze.Box
import Database.Gruze.Types

import Language.Haskell.TH
import Data.Typeable

declsObj = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass)|]
declsContainer = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzContainerClass)|]
declsOwner = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass)|]
declsSite = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzSiteClass)|]
declsOwnerContainer = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass,GrzContainerClass)|]

decl decls = do
        [d] <- decls
        -- runIO $ print d -- just to show internals
        return d

doDef :: String -> Q [Dec] -> Q [Dec]
doDef n decls = do
        d <- decl decls
        let name = mkName n
        return $ (\x -> [x]) $ case d of
                (NewtypeD cxt _ argvars (NormalC _ args) derivings) ->
                        NewtypeD cxt name argvars (NormalC name args) derivings
                        
defObj n = doDef n declsObj
defContainer n = doDef n declsContainer
defOwner n = doDef n declsOwner
defSite n = doDef n declsSite
defOwnerContainer n = doDef n declsOwnerContainer

-- Here's a CPP alternative to the above Template Haskell approach:

-- #define defSite(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzSiteClass)
-- #define defContainer(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzContainerClass)
-- #define defOwner(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass)
-- #define defObj(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass)
-- #define defOwnerContainer(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass,GrzContainerClass)