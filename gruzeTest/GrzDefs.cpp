#define defSite(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzSiteClass)
#define defContainer(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzContainerClass)
#define defOwner(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass)
#define defObj(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass)
#define defOwnerContainer(NAME) newtype NAME = NAME GrzObj deriving (Typeable,GrzAtomBoxClass,GrzObjClass,GrzOwnerClass,GrzContainerClass)
#define defRel(NAME) NAME = GrzRel "NAME"
#define CONCAT(a,b,c) a/* */b/* */c 
#define defFieldAs(NAME,TYPE,VALUE) NAME = ((GrzAtomKey { atomKey = "VALUE" }) :: CONCAT(GrzAtom,TYPE,Key) )
#define defField(NAME,TYPE) NAME = ((GrzAtomKey { atomKey = "NAME" }) :: CONCAT(GrzAtom,TYPE,Key) )
