{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Store.Gruze
import Data.Store.Gruze.Templates

import qualified Data.ByteString as BS

default (Int)

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

{-|
      This example application allows teachers to post to a special blog visible only to
      teachers and not students. Gruze does not provide one standard access control system
      but as the example below shows, you can easily create one using role objects and 
      permission relationships.
-} 

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
            -- at debug level (used in the example code below), Gruze logs a lot of 
            -- information (generated queries, object creation notices etc.)
            . (setString "grzLogFile" "D:/projects/haskell/newlog2.txt")
            
            -- location of Imagemagick convert executable on file system
            . (setString "grzConvertLocation" "D:/Program Files/imagemagick-6.3.5-q8/convert.exe")
                
    -- get handle (which opens the database connection)
    
    -- In this simple example, the handle is passed directly to all the model
    -- functions. In a more complex example, the handle could be hidden in a
    -- Reader monad or some other state monad to reduce parameter clutter          
    grzH' <- getHandle config
       
    -- delete any previous test site (and all its content)
    let testSitesQd = hasIn "subtype" ["grzTest"]
    testSites <- getBareObjs grzH' Site testSitesQd [] 0 0
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
        
    loggedInUserRole <- createRole grzH
        "loggedInUser"
        "All registered users should be given this role."
    
    -- create a collection for teacher content    
    teacherCollection <- createCollection grzH
        "Collection of content for teachers."
    
    -- create a communal teacher's blog within the teacher collection 
    teacherBlog <- createBlog grzH teacherCollection
        "A collective blog used by all teachers."
        
    -- create a collection for student content    
    studentCollection <- createCollection grzH
        "Collection of content for students."
    
    -- create a communal student's blog within the student collection 
    studentBlog <- createBlog grzH studentCollection
        "A collective blog used by all students."
    
    -- create some teachers    
    john <- createUser grzH
        "John Smith"
        "john@example.com"
        [loggedInUserRole, teacherRole]
          
    jane <- createUser grzH
        "Jane Doe"
        "jane@example.com"
        [loggedInUserRole, teacherRole]
        
    wendy <- createUser grzH
        "Wendy Inkster"
        "wendy@example.com"
        [loggedInUserRole, teacherRole]
        
    -- create a student (does not have the teacherRole)
    tom <- createUser grzH
        "Tom Chang"
        "tom@example.com"
        [loggedInUserRole]
    
    -- John posts to the teacher blog and sets view access to the teacherRole
    post <- postBlog grzH teacherBlog john teacherRole
        "My first blog post" 
        "Testing the object stre." 
        ["testing","haskell","gruze"]
        Nothing
        
    -- John edits the blog post and adds an image
    
    -- load the little Gruze monster image
    s <- BS.readFile $ (grzDataDirectory grzH) ++ "/test/gruze.png"
    
    -- convert the image to a file atom
    fa <- createFileAtom grzH "gruze.png" "image/png" s
    
    -- The following shows how to do a quick change to an object outside
    -- of an accessor function.
    
    -- The example changes the title and description and adds an image.
    
    let edits = 
            (setString "title" "My first blog post (edited)")
            . (setString "body" "Testing the object store.")
            . (setAtom "image" fa)
    
    -- Apply edits to original post and save. 
    johnPost <- saveObj grzH (edits post)
        
    putStrLn "\nNew blog post created, full output:"           
    putStrLn $ ppObjFull johnPost
    
    -- Load and display the owner object including the name field.
    -- Not all objects have owners so this is a maybe.
    postOwner <- maybeLoadOwner grzH User johnPost ["name"]
    
    putStrLn $ "Blog post owner: " ++ (fromMaybe "" $ fmap ppObjFull postOwner)              
    
    -- Wendy and Jane rate the post, and Jane leaves a comment
    
    rateBlogPost grzH post wendy 3
    rateBlogPost grzH post jane 5
    janeComment <- commentOnBlogPost grzH post jane teacherRole
        "John, thanks for checking Gruze out!"
        
    -- Get the container of the container of Jane's Comment, which should be 
    -- a Blog.
    
    -- This shows how to do queries with one of the special relationships.
    -- Notice that the withObj invocation occurs first (bottom) because it
    -- refers to the Comment and the withData invocation occurs last (top)
    -- because it refers to the Blog
    putStrLn "\nContainer of the container of Jane's comment:" 
    let ccqd = (withData ["title"])
                . (hasRel "+hasContainer") 
                . (hasRel "+hasContainer") 
                . (withObj janeComment)
    rc <- getObjs grzH Blog ccqd [] 0 0
    mapM (putStrLn . ppObjFull) rc
    
    -- Wendy the teacher posts in the teacher blog
    -- setting the access to teachers only 
    wendyPost <- postBlog grzH teacherBlog wendy teacherRole
        "Wendy's first post" 
        "Looking forward to learning about the Gruze API." 
        ["experimenting","coding","new stuff"]
        Nothing  
    
    -- Tom the student posts in the student blog
    -- setting the access to all logged-in users 
    tomPost <- postBlog grzH studentBlog tom loggedInUserRole
        "Hello world!" 
        "Blogging from a student perspective." 
        ["experimenting","cool stuff","gruze"]
        Nothing    
    
    -- Another query example, this time getting all the high Ratings (>= 4) 
    -- for John's BlogPost.
    
    -- Notice the reverse "-hasContainer" relationship, which gets all the
    -- objects within a container.
    
    putStrLn "\nAll high ratings of John's blog post:" 
    let ccqd = (withData ["value"])
                . (hasOp "value" ">=" 4)
                . (hasRel "-hasContainer") 
                . (withObj johnPost)
    rc <- getObjs grzH Rating ccqd [] 0 0
    mapM (putStrLn . ppObjFull) rc
   
    -- examples of overall aggregation (which returns a count or a (sum,count)
    -- pair rather than an object list)
         
    -- generate a report on the blog post response
    
    -- get the number of Comments
    commentCount <- getObjCount grzH
        ((hasType Comment)
        . (hasContainer post))
    
    -- get the sum and count of the value field of the relevant Ratings        
    r <- getObjAggSumCount grzH
        ((hasType Rating)
        . (hasContainer post))
        "value"
    
    let avgRating = if (snd r) == 0 
                        then 
                            "No ratings yet" 
                        else 
                            show $ (fst r) `quot` (snd r)
                            
    putStrLn $ "Report on John's blog post:\nNumber of comments: " 
        ++ (show commentCount)
        ++ "\nNumber of ratings: "
        ++ (show $ snd r)
        ++ "\nAverage rating: "
        ++ avgRating
        
    -- Examples of per object aggregation
    
    -- This returns either a (object, count) pair using getObjAggByObjCount
    -- or a (object,(sum,count)) pair using getObjAggByObjSumCount.
    
    -- In this example, we are listing all blog posts with a count of the 
    -- comments and then queries with a sum and count of the all ratings
    -- for each post, and then just the high ratings (value >= 4) for
    -- all blog posts inside blogs inside the teacher collection.
    
    -- The Gruze query system takes special care to deal with the case where
    -- the object being aggregated (in this case a comment or rating) does not
    -- exist. Notice in this example that the blog post is still returned but 
    -- with a sum and/or count of zero. Achieving this in SQL requires 
    -- LEFT JOINs and special NULL handling - details that are hidden by the 
    -- higher level Gruze query system.
    
    putStrLn $ "\nPer object aggregation example \n(count of comments per blog post): "
    
    raboc <- getObjAggByObjCount grzH Comment BlogPost
                ((hasRel "-hasContainer"))
                [] 0 0
            
    putStrLn $ concatMap (\(o,c) -> 
                ("(" ++ (ppObj o) ++ "," ++ (show c) ++ ")\n")
            ) raboc
            
    putStrLn $ "\nPer object aggregation example \n(sum and count of all ratings per blog post): "
    
    rabosc <- getObjAggByObjSumCount grzH "value" Rating BlogPost
                ((hasRel "-hasContainer"))
                [SumAsc,GuidDesc] 0 0
            
    putStrLn $ concatMap (\(o,(s,c)) -> 
                ("(" ++ (ppObj o) ++ "," ++ (show s) ++ "," ++ (show c) ++ ")\n")
            ) rabosc
            
    -- a more complex example aggregating high value ratings for blog posts
    -- that are contained in blogs inside the teacher collection
    
    putStrLn $ "\nPer object aggregation example \n(sum and count of high ratings per teacher blog post): "
    
    rabosc2 <- getObjAggByObjSumCount grzH "value" Rating BlogPost
                (   (hasOp "value" ">=" 4)
                    . (hasRel "-hasContainer") 
                    . (hasRel "-hasContainer")
                    . (hasType Blog)
                    . (hasRel "-hasContainer")
                    . (withObj teacherCollection)
                )
                [SumDesc] 0 0
            
    putStrLn $ concatMap (\(o,(s,c)) -> 
                ("(" ++ (ppObj o) ++ "," ++ (show s) ++ "," ++ (show c) ++ ")\n")
            ) rabosc2
    
    -- When searching, Tom cannot find the same content as John
    -- because students and teachers have different roles.
    
    -- Since searchForContent does not know in advance what
    -- content will be returned, these are unwrapped objects.
        
    johnContent <- searchForContent grzH john "Gruze"
    
    putStrLn "Content found by John's search for \"Gruze\" :"           
    mapM (putStrLn .  ppObj) johnContent
    
    tomContent <- searchForContent grzH tom "Gruze"
    
    putStrLn "\nContent found by Tom's search for \"Gruze\" :"           
    mapM (putStrLn .  ppObj) tomContent

newtype File = File GrzAtom
  
-- object creation and accessor functions

createSite :: GrzHandle -> String -> String -> String -> IO Site
createSite grzH subType title description =
    createObj grzH Site od
    where
        od =
            (setString "subtype" subType)
            . (setString "title" title)
            . (setString "description" title)

-- create a user and optionally add the user to roles    
createUser :: GrzHandle -> String -> String -> [Role] -> IO User
createUser grzH name email roles = do
    user <- createObj grzH User od
    mapM_ (addUserToRole grzH user) roles
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
    grantPermission grzH bp "View" role
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
    grantPermission grzH c "View" role
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
            
-- an example generic search function
-- searches for enabled content that the given user has permission to see
-- as the types are unclear, returns unwrapped objects                    
searchForContent :: GrzHandle -> User -> String -> IO [GrzObj]
searchForContent grzH user s = do
    getUnwrappedObjs grzH qd [] 0 0
    where
        qd = hasPermission user "View" ((hasSearchable s) . (hasEnabled))

            
-- Roles and permissions
            
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
addUserToRole grzH user role =
    addRel grzH "hasRole" user role
    
grantPermission :: GrzObjClass o => GrzHandle -> o -> String -> Role -> IO ()
grantPermission grzH obj perm role = do
    addRel grzH ("hasPerm" ++ perm) role obj

-- looks for indirect relationships
-- linking the user and the object through
-- the role object and the permission relationship
hasPermission :: User -> String -> (GrzQueryDef -> GrzQueryDef) -> GrzQueryDef -> GrzQueryDef            
hasPermission user perm qd =
    qd
    . (hasRel $ "+hasPerm" ++ perm)
    . (hasRel "+hasRole")
    . (withObj user)
