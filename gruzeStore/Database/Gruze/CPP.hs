{-# LANGUAGE CPP #-}
module Database.Gruze.CPP where

#define defSite(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzSiteClass)
#define defContainer(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzContainerClass)
#define defOwner(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass)
#define defObj(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass)
#define defOwnerContainer(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass,GrzContainerClass)
