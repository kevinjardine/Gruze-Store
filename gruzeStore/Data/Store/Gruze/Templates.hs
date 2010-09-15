{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Store.Gruze.Templates (

defObj, defContainer, defOwner, defSite, defOwnerContainer

) where

import Data.Store.Gruze.Box
import Data.Store.Gruze.Types

import Language.Haskell.TH
import Data.Typeable

declsObj = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass)|]
declsContainer = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzContainerClass)|]
declsOwner = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass)|]
declsSite = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzSiteClass)|]
declsOwnerContainer = [d|newtype TempDecl = TempDecl GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass,GrzContainerClass)|]

decl decls = do
        [d] <- decls
        runIO $ print d -- just to show internals
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