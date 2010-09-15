{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-} 

import Data.Store.Gruze
import Data.Store.Gruze.Templates

import Data.Maybe
import Data.Typeable
import qualified Data.ByteString as BS

-- define some type safe wrappers

-- Some experimental Template Haskell to remove the boilerplate.
-- These definitions are here because TH requires that definitions occur
-- before they are referenced.

-- The model functions occur after main().

-- sites

$(defSite "Site")

-- owners

$(defOwner "User")

-- containers

$(defContainer "Collection")
$(defContainer "Blog")
$(defContainer "BlogPost")

--other objects

$(defObj "Role")
$(defObj "Comment")
$(defObj "Rating")

main = do

    {-
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
            . (setString "grzConvertLocation" "D:/Program Files/imagemagick-6.3.5-q8/convert.exe")
        
    {-|
      This example application allows teachers to post to a special blog visible only to
      teachers and not students. Gruze does not provide one standard access control system
      but as the example below shows, you can easily create one using role objects and 
      permission relationships.
    -} 
                
    -- get handle (which opens the database connection)
    
    -- In this simple example, the handle is passed directly to all the model
    -- functions. In a more complex example, the handle could be hidden in a
    -- Reader monad or some other state monad to reduce parameter clutter          
    grzH' <- getHandle config
       
    -- delete any previous test site (and all its content)
    let testSitesQd = hasStringIn "subtype" ["grzTest"]
    testSites <- getBareObjs grzH' Site testSitesQd 0 0
    mapM_ (delObj grzH') testSites
    
    -- create a site to hold all the test content    
    site <- createSite grzH'
        "grzTest"
        "Test site"
        "Used for adding content to test out the Gruze object store."
    
    putStrLn "\nNew site created:"     
    putStrLn $ ppObj site
    
    -- set the default site, log level and the thumb definitions
    let grzH = (setThumbDefs [("standard","80x80"),("small","32x32")])
               . (setLogLevel DebugLogLevel)
               . (setDefaultSite site)
               $ grzH'
                 
    -- make some fields searchable
    setSearchable grzH BlogPost ["title","body","tags"]
    setSearchable grzH Blog ["title"]
    setSearchable grzH Comment ["body"]
    
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
    post <- postBlog grzH teacherBlog john teacherRole
        "My first blog post" 
        "Testing the Gruze object stre." 
        ["testing","haskell","gruze"]
        Nothing
        
    -- John edits the blog post and adds an image
    
    -- load the little Gruze monster image
    s <- BS.readFile $ (grzDataDirectory grzH) ++ "/test/gruze.png"
    
    -- convert the image to a file atom
    fa <- createFileAtom grzH "gruze.png" "image/png" s
    
    let edits = 
            (setString "title" "My first blog post (edited)")
            . (setString "body" "Testing the Gruze object store.")
            . (setAtom "image" fa)
    
    -- apply edits to original post and save    
    revisedPost <- saveObj grzH (edits post)
        
    putStrLn "\nNew blog post created, full output:"           
    putStrLn $ ppObjFull revisedPost
    
    -- load the owner object including the name field
    -- not all objects have owners so this is a maybe
    postOwner <- maybeLoadOwner grzH User revisedPost ["name"]
    
    putStrLn $ "Blog post owner: " ++ (fromMaybe "" $ fmap ppObjFull postOwner)              
    
    -- Wendy and Jane rate the post, and Jane leaves a comment
    
    rateBlogPost grzH post wendy 3
    rateBlogPost grzH post jane 5
    commentOnBlogPost grzH post jane teacherRole
        "John, thanks for checking Gruze out!"
        
    -- generate a report on the blog post response
    
    -- get the number of comments
    commentCount <- getObjCount grzH
        ((hasType Comment)
        . (hasContainer post))
    
    -- get the sum and count of the value field of the relevant ratings        
    r <- getObjAggSumCount grzH
        ((hasType Rating)
        . (hasContainer post))
        "value"
        
    putStrLn $ "Report on John's blog post:\nNumber of comments: " 
        ++ (show commentCount)
        ++ "\nNumber of ratings: "
        ++ (show $ snd r)
        ++ "\nAverage rating: "
        ++ (show $ ((fst r) `quot` (snd r)))
    
    -- when searching, Tom cannot find the same content as John
    -- because students and teachers have different roles
    
    -- since seatchForContent does not know in advance what
    -- content will be returned, these are unwrapped objects
        
    johnContent <- searchForContent grzH john "Gruze"
    
    putStrLn "\nContent found by John's search for \"Gruze\" :"           
    mapM (putStrLn .  ppObj) johnContent
    
    tomContent <- searchForContent grzH tom "Gruze"
    
    putStrLn "\nContent found by Tom's search for \"Gruze\" :"           
    mapM (putStrLn .  ppObj) tomContent

newtype File = File GrzAtom
  
-- model functions

createSite :: GrzHandle -> String -> String -> String -> IO Site
createSite grzH subType title description =
    createObj grzH Site od
    where
        od =
            (setString "subtype" subType)
            . (setString "title" title)
            . (setString "description" title)

-- create a user and optionally add the user to a role    
createUser :: GrzHandle -> String -> String -> Maybe Role -> IO User
createUser grzH name email maybeRole = do
    user <- createObj grzH User od
    case maybeRole of
        Just role -> do
            addUserToRole grzH user role
            return user
        Nothing ->
            return user
    where
        od =
            (setSite (grzDefaultSite grzH))
            . (setString "name" name)
            . (setString "email" email)
   
createBlog :: GrzHandle -> Collection -> String -> IO Blog
createBlog grzH c title =
    createObj grzH Blog od
    where
        od =
            (setSite (grzDefaultSite grzH))
            . (setContainer c)
            . (setString "title" title)
                
createCollection :: GrzHandle -> String -> IO Collection
createCollection grzH title = 
    createObj grzH Collection od
    where
        od =
            (setSite (grzDefaultSite grzH))
            . (setString "title" title)

-- posts to the blog and sets the permission to the specified role
postBlog :: GrzHandle -> Blog -> User -> Role -> String -> String -> [String] -> Maybe File -> IO BlogPost
postBlog grzH blog user role title body tags image = do
    bp <- createObj grzH BlogPost od
    grantPermission grzH bp "view" role
    return bp
    where
        od =
            (setSite (grzDefaultSite grzH))
            . (setOwner user)
            . (setContainer blog)
            . (setString "title" title)
            . (setString "body" body)
            . (setStringList "tags" tags)
            . case image of
                Just (File im) -> setAtom "image" im
                otherwise -> id
                    
-- comments on a blog post and sets the permission to the specified role
commentOnBlogPost :: GrzHandle -> BlogPost -> User -> Role -> String -> IO Comment
commentOnBlogPost grzH post commenter role comment = do
    c <- createObj grzH Comment od
    grantPermission grzH c "view" role
    return c
    where
        od =
            (setSite (grzDefaultSite grzH))
            . (setOwner commenter)
            . (setContainer post)
            . (setString "body" comment)
                
-- rates a blog post
rateBlogPost :: GrzHandle -> BlogPost -> User -> Int -> IO ()
rateBlogPost grzH post rater rating = do
    createObj grzH Rating od
    return ()
    where
        od =
            (setSite (grzDefaultSite grzH))
            . (setOwner rater)
            . (setContainer post)
            . (setInt "value" rating)

-- searches for content that the given user has permission to see                    
searchForContent :: GrzHandle -> User -> String -> IO [GrzObj]
searchForContent grzH user s = do
    getUnwrappedObjs grzH qd 0 0
    where
        qd =
            (hasPermission user "view")
            . (hasSearchable s)
            
-- in the simple roles and permissions system created below,
-- users are associated with a number of possible roles and
-- permissions (eg. "view") are granted to each role
                
createRole :: GrzHandle -> String -> String -> IO Role
createRole grzH title description = 
    createObj grzH Role od
    where
        od =
            (setSite (grzDefaultSite grzH))
            . (setString "title" title)
            . (setString "description" description)
                               
addUserToRole :: GrzHandle -> User -> Role -> IO ()
addUserToRole grzH (User user) (Role role) =
    addRel grzH "hasRole" user role
    
grantPermission :: GrzObjClass o => GrzHandle -> o -> String -> Role -> IO ()
grantPermission grzH obj perm role = do
    addRel grzH ("perm:" ++ perm) role obj

-- hasIndRel looks for indirect relationships
-- linking the user and the object through
-- the role object and the permission relationship            
hasPermission user perm =
    hasIndRel "hasRole" ("perm:" ++ perm) [user]
