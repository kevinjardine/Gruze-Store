{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Store.Gruze.Container
import Data.Store.Gruze.IO
import Data.Store.Gruze.QueryDef

import Data.Maybe

main = do

    {-|
      Redefine the config parameters below to match your own MySQL and 
      file system configuration.
    -} 
     
    let config =
            -- the ODBC driver string 
            (setString "grzDBDriver" "{MySQL ODBC 3.51 Driver}")
            
            -- db server location (usually localhost)
            . (setString "grzDBServer" "localhost")
            
            -- database name
            . (setString "grzDBDatabase" "gruze2")
            
            -- database user name
            . (setString "grzDBUID" "gruze")
            
            -- database password
            . (setString "grzDBPassword" "gtest")
            
            -- location of data directory on file system (must be writable by Haskell)
            . (setString "grzDataDirectory" "D:/projects/haskell/storedata")
            
            -- location of log file on file system (must be writable by Haskell)
            -- currently Gruze logs a lot of information (generated queries etc.)
            . (setString "grzLogFile" "D:/projects/haskell/newlog2.txt")
            
            -- location of Imagemagick convert executable on file system
            -- Note: this is not used in the example below so you can
            -- set it to the empty string
            . (setString "grzConvertLocation" "\"D:/program files/imagemagick-6.3.5-q8/convert.exe\"")
        
    {-|
      This example application allows teachers to post to a special blog visible only to
      teachers and not students. Gruze does not provide one standard access control system
      but as the example below shows, you can easily create one using role objects and 
      permission relationships.
    -} 
                
    -- get handle (which opens the database connection) 
    -- in this simple example, the handle is passed
    -- directly to all the model functions
    -- in a more complex example, it could be hidden in
    -- a Reader monad or some other state monad
    -- to reduce parameter clutter          
    grzH' <- getHandle config
    
    -- delete any previous test site (and all its content)
    let testSitesQd = (hasType "site") . (hasStringIn "subtype" ["grzTest"])
    testSites <- getBareObjs grzH' testSitesQd 0 0
    mapM_ (delObj grzH') testSites
    
    -- create a site to hold all the test content    
    site <- createSite grzH'
        "grzTest"
        "Test site"
        "Used for adding content to test out the Gruze object store."
    
    putStrLn "\nNew site created:"     
    putStrLn $ ppObj $ toObj site
    
    -- make this the default site to be used by other objects
    let grzH = setDefaultSite grzH' site

    -- make some fields searchable
    setSearchable grzH "blogPost" ["title","body","tags"]
    setSearchable grzH "blog" ["title"]
    setSearchable grzH "comment" ["body"]
    
    -- create roles for teachers and students
    teacherRole <- createRole grzH
        "teacher"
        "All teachers should be given this role."
        
    studentRole <- createRole grzH
        "student"
        "All students should be given this role."
    
    -- create a collection for teacher content    
    teacherCollection <- createCollection grzH
        "Collection of content for teachers."
    
    -- create a communal teacher's blog within the teacher collection 
    teacherBlog <- createBlog grzH teacherCollection
        "A collective blog used by all teachers."
    
    -- create some teachers    
    john <- createUser grzH
        "John Smith"
        "john@example.com"
        (Just teacherRole)
          
    jane <- createUser grzH
        "Jane Doe"
        "jane@example.com"
        (Just teacherRole)
        
    wendy <- createUser grzH
        "Wendy Inkster"
        "wendy@example.com"
        (Just teacherRole)
        
    -- create a student
    tom <- createUser grzH
        "Tom Chang"
        "tom@example.com"
        (Just studentRole)
    
    -- John posts to the teacher blog
    post <- postBlog grzH teacherBlog john
        "My first blog post" 
        "Testing the Gruze object store." 
        ["testing","haskell","gruze"]
        Nothing
        
    putStrLn "\nNew blog post created, full output:"           
    putStrLn $ ppObjFull (toObj post)
    
    -- Wendy and Jane rate the post, and Jane leaves a comment
    
    rateBlogPost grzH post wendy 3
    rateBlogPost grzH post jane 5
    commentOnBlogPost grzH post jane 
        "John, thanks for checking Gruze out!"
        
    -- generate a report on the blog post response
    
    -- get the number of comments
    commentCount <- getObjCount grzH
        ((hasType "comment")
        . (hasContainer (toObj post)))
    
    -- get the sum and count of the value field of the relevant ratings        
    r <- getObjAggSumCount grzH
        ((hasType "rating")
        . (hasContainer (toObj post)))
        "value"
        
    putStrLn $ "Report on John's blog post:\nNumber of comments: " 
        ++ (show commentCount)
        ++ "\nNumber of ratings: "
        ++ (show $ snd r)
        ++ "\nAverage rating: "
        ++ (show $ ((fst r) `quot` (snd r)))
    
    -- when searching, Tom cannot find the same content as John
    -- because students and teachers have different roles
        
    johnContent <- searchForContent grzH john "Gruze"
    
    putStrLn "\nContent found by John's search for \"Gruze\" :"           
    mapM (putStrLn .  ppObj) johnContent
    
    tomContent <- searchForContent grzH tom "Gruze"
    
    putStrLn "\nContent found by Tom's search for \"Gruze\" :"           
    mapM (putStrLn .  ppObj) tomContent
    
-- define some type safe wrappers
-- (the ToGrzObj class provides a toObj function)
   
newtype Site = Site GrzObj deriving ToGrzObj
newtype User = User GrzObj deriving ToGrzObj
newtype Role = Role GrzObj deriving ToGrzObj
newtype Collection = Collection GrzObj deriving ToGrzObj
newtype Blog = Blog GrzObj deriving ToGrzObj
newtype BlogPost = BlogPost GrzObj deriving ToGrzObj
newtype Comment = Comment GrzObj deriving ToGrzObj

newtype File = File GrzAtom
  
-- model functions

createSite :: GrzHandle -> String -> String -> String -> IO Site
createSite grzH subType title description =
    fmap Site (createObj grzH od)
    where
        od =
            (setType "site")
            . (setString "subtype" subType)
            . (setString "title" title)
            . (setString "description" title)

setDefaultSite :: GrzHandle -> Site -> GrzHandle
setDefaultSite grzH (Site site) =               
    grzH {grzDefaultSite = site}

-- create a user and optionally add the user to a role    
createUser :: GrzHandle -> String -> String -> Maybe Role -> IO User
createUser grzH name email maybeRole = do
    user <- fmap User (createObj grzH od)
    case maybeRole of
        Just role -> do
            addUserToRole grzH user role
            return user
        Nothing ->
            return user
    where
        od =
            (setType "user")
            . (setSite (grzDefaultSite grzH))
            . (setString "name" name)
            . (setString "email" email)
   
createBlog :: GrzHandle -> Collection -> String -> IO Blog
createBlog grzH (Collection c) title =
    fmap Blog (createObj grzH od)
    where
        od =
            (setType "blog")
            . (setSite (grzDefaultSite grzH))
            . (setContainer c)
            . (setString "title" title)
                
createCollection :: GrzHandle -> String -> IO Collection
createCollection grzH title = 
    fmap Collection (createObj grzH od)
    where
        od =
            (setType "collection")
            . (setSite (grzDefaultSite grzH))
            . (setString "title" title)

-- posts to the blog and sets the permission to teachers only
postBlog :: GrzHandle -> Blog -> User -> String -> String -> [String] -> Maybe File -> IO BlogPost
postBlog grzH (Blog blog) (User owner) title body tags image = do
    bp <- fmap BlogPost (createObj grzH od)
    grantPermission grzH (toObj bp) "view" "teacher"
    return bp
    where
        od =
            (setType "blogPost")
            . (setSite (grzDefaultSite grzH))
            . (setOwner owner)
            . (setContainer blog)
            . (setString "title" title)
            . (setString "body" body)
            . (setStringList "tags" tags)
            . case image of
                Just (File im) -> setAtom "image" im
                otherwise -> id
                    
-- comments on a blog post and sets the permission to teachers only
commentOnBlogPost :: GrzHandle -> BlogPost -> User -> String -> IO Comment
commentOnBlogPost grzH (BlogPost post) (User commenter) comment = do
    c <- fmap Comment (createObj grzH od)
    grantPermission grzH (toObj c) "view" "teacher"
    return c
    where
        od =
            (setType "comment")
            . (setSite (grzDefaultSite grzH))
            . (setOwner commenter)
            . (setContainer post)
            . (setString "body" comment)
                
-- rates a blog post
rateBlogPost :: GrzHandle -> BlogPost -> User -> Int -> IO ()
rateBlogPost grzH (BlogPost post) (User rater) rating = do
    createObj grzH od
    return ()
    where
        od =
            (setType "rating")
            . (setSite (grzDefaultSite grzH))
            . (setOwner rater)
            . (setContainer post)
            . (setInt "value" rating)

-- searches for content that the given user has permission to see                    
searchForContent :: GrzHandle -> User -> String -> IO [GrzObj]
searchForContent grzH (User user) s = do
    getObjs grzH qd 0 0
    where
        qd =
            (hasPermission user "view")
            . (hasSearchable s)
            
-- in the simple roles and permissions system created below,
-- users are associated with a number of possible roles and
-- permissions (eg. "view") are granted to each role
                
createRole :: GrzHandle -> String -> String -> IO Role
createRole grzH title description = 
    fmap Role (createObj grzH od)
    where
        od =
            (setType "role")
            . (setSite (grzDefaultSite grzH))
            . (setString "title" title)
            . (setString "description" description)
                               
addUserToRole :: GrzHandle -> User -> Role -> IO ()
addUserToRole grzH (User user) (Role role) =
    addRel grzH "hasRole" user role
    
grantPermission :: GrzHandle -> GrzObj -> String -> String -> IO ()
grantPermission grzH obj perm role = do
    objs <- getObjs grzH qd 0 1
    if (not $ null objs) && (isValidObj $ head objs)
        then
            addRel grzH ("perm:" ++ perm) (head objs) obj
        else
            return ()
    where
        qd =
            (hasType "role") 
            . (hasStringIn "title" [role])

-- hasIndRel looks for indirect relationships
-- linking the user and the object through
-- the role object and the permission relationship            
hasPermission user perm =
    hasIndRel "hasRole" ("perm:" ++ perm) [user]