module Data.Store.Gruze.Query

where

-- internal functions to convert and run query definitions

import Data.Store.Gruze.Types
import Data.Store.Gruze.Box
import Data.Store.Gruze.DBTypes
import Data.Store.Gruze.Handles
import Data.Store.Gruze.Utility

import Database.HDBC
import Database.HDBC.ODBC
import Data.List (intercalate, foldl')
import qualified Data.Map as Map
import Data.Maybe

-- some useful pure query-related functions

valuesToSql :: GrzQueryValues -> [SqlValue]
valuesToSql values = ((map toSql (fst values)) ++ (map toSql (snd values)))

-- adds an extra bit to the query string
addToQuery :: String 
    -> Maybe GrzQuery    
    -> Maybe GrzQuery
addToQuery extra q = 
    case q of
        Just (((qs,qt),hd),v) -> Just (((qs++extra,qt),hd),v) 
        Nothing -> Nothing

queryResultToCount :: ([[SqlValue]],([(String,Int)],[[SqlValue]]))  -- ^ the query result
    -> Int
queryResultToCount ([[v]],_) = (fromSql v)::Int
queryResultToCount _ = 0

queryResultToSumCount :: ([[SqlValue]],([(String,Int)],[[SqlValue]]))  -- ^ the query result
    -> (Int, Int)
    
queryResultToSumCount ([[_,SqlNull]],_) = (0,0)
queryResultToSumCount ([[SqlNull,_]],_) = (0,0)
queryResultToSumCount ([[s,c]],_) = ((fromSql s)::Int,(fromSql c)::Int)
queryResultToSumCount _ = (0,0)

populateObj m [oid,ot,otc,otu,oo,oc,os,e] =
    GrzObjFull {
        objID = guid,
        objType = (fromSql ot) :: String,
        objTimeCreated = (fromSql otc) :: Int,
        objTimeUpdated = (fromSql otu) :: Int,
        objOwner = GrzObjID ((fromSql oo) :: Int),
        objContainer = GrzObjID ((fromSql oc) :: Int),
        objSite = GrzObjID ((fromSql os) :: Int),
        objEnabled = enabled,
        objMetadata = case Map.lookup guid m of
                        Just b -> b
                        Nothing -> emptyAtomBox
    }
    where 
        guid = (fromSql oid) :: Int
        enabled = if ((fromSql e) :: Int) == 1 then True else False
           
queryRowToObj :: Map.Map Int GrzAtomBox -> [SqlValue] -> GrzObj
queryRowToObj m r = populateObj m r    

queryResultToAggByObjCount :: GrzObjClass o =>
     (GrzObj -> o)                                      -- ^ type wrapper
    -> ([[SqlValue]],([(String,Int)],[[SqlValue]]))     -- ^ query result
    -> [(o,Int)]                                        -- returns list of (obj, count) pairs
queryResultToAggByObjCount w (r, (n,mr)) = map (queryRowToAggByObjCount w mq) r
    where
        mq =  metadataQueryToAtomBoxDict n mr
        
queryResultToAggByObjSumCount :: GrzObjClass o =>
     (GrzObj -> o)                                      -- ^ type wrapper
    -> ([[SqlValue]],([(String,Int)],[[SqlValue]]))     -- ^ query result
    -> [(o,(Int,Int))]                                    -- returns list of (obj, count) pairs
queryResultToAggByObjSumCount w (r, (n,mr)) = map (queryRowToAggByObjSumCount w mq) r
    where
        mq =  metadataQueryToAtomBoxDict n mr
                        
queryRowToAggByObjCount :: GrzObjClass o =>
    (GrzObj -> o)               -- ^ type wrapper
    -> Map.Map Int GrzAtomBox   -- ^ metadata 
    -> [SqlValue]               -- row from database
    -> (o,Int)                  -- resulting (obj,count) pair
queryRowToAggByObjCount w m r =
    (obj, count)
    where 
        count = (fromSql (r !! 8)) :: Int
        obj = w $ populateObj m (take 8 r)

queryRowToAggByObjSumCount :: GrzObjClass o =>
    (GrzObj -> o)               -- ^ type wrapper
    -> Map.Map Int GrzAtomBox   -- ^ metadata 
    -> [SqlValue]               -- row from database
    -> (o,(Int,Int))            -- resulting (obj,(sum,count)) pair
queryRowToAggByObjSumCount w m r =
    (obj, (qsum,count))
    where 
        count = (fromSql (r !! 8)) :: Int
        qs = r !! 9 
        qsum = if qs == SqlNull then 0 else ((fromSql qs) :: Int)
        obj = w $ populateObj m (take 8 r)
            
queryResultToObjs :: ([[SqlValue]],([(String,Int)],[[SqlValue]]))
    -> [GrzObj]
queryResultToObjs (r, (n,mr)) = map (queryRowToObj mq) r
    where
        mq =  metadataQueryToAtomBoxDict n mr

needToString :: GrzQueryDefItem -> [String]
needToString (GrzQDNeeds x _) = [x]
needToString _ = []

getQueryNeeds :: [GrzQueryDefItem] -> [String]
getQueryNeeds q = concatMap needToString q

joinToString :: Int -> Maybe Int -> GrzQueryDefItem -> [String]
joinToString m (Just _) (GrzQDJoin n x) = if n >= m-1 then ["LEFT JOIN " ++ x] else ["INNER JOIN " ++ x]
joinToString m Nothing (GrzQDJoin n x) = ["INNER JOIN " ++ x]
joinToString _ _ _ = []

getQueryJoins :: [GrzQueryDefItem] -> Int -> Maybe Int -> [String]
getQueryJoins q m maybeAggByObjQueryTypeID = concatMap (joinToString m maybeAggByObjQueryTypeID) q

whereToString :: Int -> Maybe Int -> GrzQueryDefItem -> [String]
whereToString m (Just _) (GrzQDWhere n s) = 
    if n >= m-1
        then [" ((obj" ++ (show n) ++ ".guid IS NULL) OR (" ++ s ++ ")) "]
        else [s]
whereToString m Nothing (GrzQDWhere n s) = [s]
whereToString _ _ _ = []

getQueryWheres :: [GrzQueryDefItem] -> Int -> Maybe Int -> [String]
getQueryWheres q m maybeAggByObjQueryTypeID = concatMap (whereToString m maybeAggByObjQueryTypeID) q

getQueryType :: [GrzQueryDefItem] -> GrzQueryType
getQueryType q = case concatMap isQueryType q of
                        [] -> GrzQTFull
                        (x:xs) -> x
isQueryType :: GrzQueryDefItem -> [GrzQueryType]
isQueryType (GrzQDType t) = [t]
isQueryType _ = []

getQueryAgg :: [GrzQueryDefItem] -> String
getQueryAgg q = case concatMap isQueryAgg q of
                        [] -> "obj0.guid"
                        (x:xs) -> x
isQueryAgg :: GrzQueryDefItem -> [String]
isQueryAgg (GrzQDAgg s) = [s]
isQueryAgg _ = []


-- Query IO functions
       
grzCreateQuery :: GrzHandle -> (GrzQueryDef -> GrzQueryDef) -> IO (Maybe GrzQuery)
grzCreateQuery grzH q = grzGetQuery grzH (q ((0,0), []))

-- Derives the query string, looking up any string handles in the database.

-- If any string handle lookups fail, this function returns Nothing;
-- otherwise it returns the list of need names and the actual query string.

-- Much of the complexity of this function comes from dealing with aggregation,
-- which in some cases requires LEFT JOINS and special null value handling
-- as the objects being aggregated do not necessarily exist (and
-- so in that case should return a count and sum of zero).

grzGetQuery :: GrzHandle -> GrzQueryDef -> IO (Maybe GrzQuery)
grzGetQuery grzH ((m,n), q) =
    do
        maybeFrags <- grzQueryWhereFragments grzH q m maybeAggByObjQueryTypeID
        if isNothing maybeFrags
            then
                return Nothing
            else do
                let justFrags = fromJust maybeFrags
                let qv = map snd justFrags
                let d = concatMap (snd . fst) justFrags
                let frags = map (fst . fst) justFrags
                return $ Just (
                    (((prefix
                    ++ " "
                    ++ (intercalate " " (reverse $ getQueryJoins q m maybeAggByObjQueryTypeID))
                    ++ aggNameJoin
                    ++ (if (null whereBit) && (null frags)
                            then ""
                            else (" WHERE " 
                                ++ (intercalate " AND " whereBit)
                                ++ (if (null frags) || (null whereBit) then "" else " AND ")
                                ++ (intercalate " AND " frags))
                        )
                    ++ aggTypeWhere
                    ++ aggNameWhere
                    ++ " " 
                    ++ suffix), queryType), d),
                   (needs, (concatMap fst qv, concatMap snd qv)) )
    where
        whereBit = getQueryWheres q m maybeAggByObjQueryTypeID
        needs = getQueryNeeds q
        queryType = getQueryType q
        maybeAggByObjQueryTypeID = case queryType of
                                GrzQTAggByObjCount i -> Just i
                                GrzQTAggByObjSumCount i _ -> Just i
                                otherwise -> Nothing
        maybeAggByObjQueryNameID = case queryType of
                                GrzQTAggByObjSumCount _ j -> Just j
                                otherwise -> Nothing
        -- need to add some sanity checking in here in case m is zero?
        agg = "obj" ++ (show $ if isJust maybeAggByObjQueryTypeID 
                                then m-1 
                                else m)
                    ++ ".guid"
        -- possibly add type and metadata constraints for the objects being aggregated
        aggTypeWhere = case maybeAggByObjQueryTypeID of
                        Just i -> " AND obj" ++ (show (m-1)) 
                            ++ ".objectType = " ++ (show i)
                        Nothing -> ""
        aggNameJoin = case maybeAggByObjQueryNameID of 
                        Just _ -> " LEFT JOIN metadata ma ON (obj" 
                            ++ (show m) ++ ".guid = ma.objectGuid) "
                        Nothing -> ""
        aggNameWhere = case maybeAggByObjQueryNameID of 
                        Just j -> " AND ((ma.nameId IS NULL) OR (ma.nameId = " ++ (show j) 
                            ++ " AND ma.metadataType = 0)) " 
                        Nothing -> ""
        sqlIndex = case queryType of
                    GrzQTCount                  -> "count"
                    GrzQTID                     -> "guid"
                    GrzQTFull                   -> "full"
                    GrzQTAggCount               -> "agg_count"
                    GrzQTAggSumCount            -> "agg_sumcount"
                    GrzQTAggByObjCount _        -> "aggbyobj_count"
                    GrzQTAggByObjSumCount _ _   -> "aggbyobj_sumcount"
        sqlFrags = fromJust $ lookup sqlIndex sqlDict    
        prefix = (fst sqlFrags) 
                    ++ if queryType `elem` [GrzQTAggCount,GrzQTAggSumCount] 
                        then "" 
                        else if isJust maybeAggByObjQueryNameID 
                                then
                                    "SELECT " ++ agg ++ " AS objGuid, count(ma.id) AS grzCount, "
                                        ++ "sum(ma.integerValue) AS grzSum FROM objects obj0 "
                                else
                                    "SELECT " ++ agg ++ " AS objGuid, count(obj" ++ (show m) 
                                        ++ ".guid) AS grzCount FROM objects obj0 "
        suffix = (if queryType `elem` [GrzQTAggCount,GrzQTAggSumCount] 
                        then ""
                        else " GROUP BY " ++ agg ++ " ")
                    ++ (snd sqlFrags)
                    
sqlDict = [
    ("full",(
        "SELECT q1.objGuid, n.string, timeCreated, timeUpdated, ownerGuid, containerGuid, siteGuid, enabled FROM (",
        ") as q1 INNER JOIN objects obje ON (obje.guid = q1.objGuid) INNER JOIN names n ON (n.id = obje.objectType)"
        )
    ),
    ("guid", (
        "",
        ""
        )
    ),
    ("count", (
        "SELECT count(DISTINCT q1.objGuid) as total FROM (",
        ") as q1"
        )
    ),
    ("agg_count", (
        "SELECT count(DISTINCT obj0.guid) as total FROM objects obj0",
        ""
        )
    ),
    ("agg_sumcount", (
        "SELECT sum(m0_0.integerValue) as total, count(DISTINCT obj0.guid) as count FROM objects obj0",
        ""
        )
     ),
     ("aggbyobj_count", (
        "SELECT q1.objGuid, n.string, timeCreated, timeUpdated, ownerGuid, containerGuid, siteGuid, enabled, q1.grzCount FROM (",
        ") as q1 INNER JOIN objects obje ON (obje.guid = q1.objGuid) INNER JOIN names n ON (n.id = obje.objectType)"
        )
     ),
     ("aggbyobj_sumcount", (
        "SELECT q1.objGuid, n.string, timeCreated, timeUpdated, ownerGuid, containerGuid, siteGuid, enabled, q1.grzCount, q1.grzSum FROM (",
        ") as q1 INNER JOIN objects obje ON (obje.guid = q1.objGuid) INNER JOIN names n ON (n.id = obje.objectType)"
        )
     )
  ]   
                   
grzQueryWhereFragments :: GrzHandle 
    -> [GrzQueryDefItem]
    -> Int
    -> Maybe Int
    -> IO (Maybe [((String,[(String,Int)]), GrzQueryValues)])
grzQueryWhereFragments grzH q m maybeAggByObjQueryTypeID = do
    r <- mapM (grzHandleWhereQueryFragment grzH m maybeAggByObjQueryTypeID) frags
    if and $ map isJust r
        then
            return $ Just (map fromJust r)
        else
            return Nothing
    where
        frags = filter (\x -> case x of
                                GrzQDWhereFrags _ _ -> True
                                otherwise -> False
                       ) q

grzHandleWhereQueryFragment :: GrzHandle
    -> Int
    -> Maybe Int
    -> GrzQueryDefItem 
    -> IO (Maybe ((String,[(String,Int)]), GrzQueryValues))
grzHandleWhereQueryFragment grzH m maybeAggByObjQueryTypeID (GrzQDWhereFrags n items) = do
    r <- mapM (qwfItemToMaybeString grzH) items
    if and $ map isJust r
        then do
            let j = map fromJust r
            let s = concatMap (fst . fst) j
            let s' = case maybeAggByObjQueryTypeID of
                        (Just _) -> if m == n
                                        then
                                            " ((obj" ++ (show n) ++ ".guid IS NULL) OR (" ++ s ++ ")) "
                                        else
                                            s
                        Nothing -> s 
            let d = concatMap (snd . fst) j
            let v = map snd j            
            return $ Just ((s',d), (concatMap fst v, concatMap snd v))
        else
            return Nothing

qwfItemToMaybeString :: GrzHandle
    -> GrzQDWFItem 
    -> IO (Maybe ((String,[(String,Int)]), GrzQueryValues))  
qwfItemToMaybeString grzH item =
    case item of
        GrzQDString s -> return $ Just ((s,[]),([],[]))
        GrzQDName s -> do
                        r <- maybeGetStringHandle grzH s
                        case r of
                            Just h -> return $ Just ((show $ snd h, [h]),([],[]))
                            Nothing -> return $ Nothing
        GrzQDNameList ss -> do
                                r <- maybeGetStringHandles grzH ss
                                case r of
                                    Just hs -> return $ Just ((intercalate "," (map (show . snd) hs),hs),([],[]))
                                    Nothing -> return $ Nothing
        GrzQDAtomClause valid ic bc sc i b s -> do
                                            if valid
                                                then
                                                    if null i && null b && null s
                                                        then
                                                            return $ Just (("",[]),([],[]))
                                                        else do
                                                            let ic2 = if null ic then [] else [ic]
                                                            let bc2 = if null bc then [] else [bc]
                                                            let sc2 = if null sc then [] else [sc]
                                                            let c = intercalate " OR " $ concat [ic2, bc2, sc2]
                                                            return $ Just ((" (" ++ c ++ ")",[]), (i ++ b, s))
                                                else
                                                    return $ Nothing
                                                    
-- runQuery converts a query request into object and metdata results
runQuery :: GrzHandle 
    -> Maybe GrzQuery 
    -> IO ([[SqlValue]],([(String,Int)],[[SqlValue]]))
runQuery grzH (Just (((query,queryType),handleDict),(needs,values))) = 
    do
        -- get the objects
        r <- grzQuery grzH query (valuesToSql values)

        -- get the metadata if it was requested, this is a full query, 
        -- and some objects were retrieved
        if (queryType /= GrzQTFull) || (null needs) || (null r)
            then
                return (r, (handleDict,[]))
            else do
                -- first step - get the extra string handles in needs but not in dict
                let stillNeeded = filter (\x -> x `notElem` (map fst handleDict)) needs
                hs <- mapM (maybeGetStringHandle grzH) stillNeeded
                let hd = handleDict ++ (map fromJust (filter isJust hs))
                
                -- newNeeds contains names with existing string handles
                let newNeeds = filter (\x -> x `elem` (map fst hd)) needs
                
                -- nhd contains all the available string handles for the requested data names
                let nhd = filter (\x -> (fst x) `elem` needs) hd
                if null nhd
                    then
                        -- oops, all the requested data names were invalid, so return no metadata
                        return (r, (handleDict,[]))
                    else do
                        -- ok, we have string handles for the names of the metadata to retrieve
                        -- so build the query to get the data
                        let startBit = "SELECT m.* FROM metadata m WHERE"
                        let needsBit = " m.nameId IN (" ++ (intercalate "," (map (show . snd) nhd)) ++ ") AND "
                        let objBit = " m.objectGuid IN (" ++ (intercalate "," (map show $ getObjIDListFromQueryResult r)) ++ ") "
                        let needsQuery = startBit ++ needsBit ++ objBit
                        r2 <- grzQuery grzH needsQuery []
                        return (r, (hd, r2))
                                
runQuery grzH Nothing = return ([],([],[]))

getObjIDListFromQueryResult r = map (\x -> ((fromSql (head x)) :: Int)) r

metadataQueryToAtomBoxDict :: [(String,Int)] -> [[SqlValue]] -> Map.Map Int GrzAtomBox
metadataQueryToAtomBoxDict hd rs = foldl' accumBoxes Map.empty ps
    where
        rhd = map (\x -> (snd x, fst x)) hd
        ps = catMaybes (map (metadataQueryRowToPair rhd) rs)
        
accumBoxes :: Map.Map Int GrzAtomBox -> (Int, (String, [GrzAtom])) -> Map.Map Int GrzAtomBox
accumBoxes m (i,p) =
    case Map.lookup i m of
        Just bs -> Map.insert i (addAtomPair p bs) m
        Nothing -> Map.insert i (addAtomPair p emptyAtomBox) m

metadataQueryRowToPair :: [(Int,String)] -> [SqlValue] -> Maybe (Int, (String, [GrzAtom]))
metadataQueryRowToPair rhd [_, objectGuid, metadataType, nameId, integerValue, stringValue] =
    case lookup ((fromSql nameId) :: Int) rhd of
        Just s -> case (fromSql metadataType) :: Int of
                    0 -> Just (guid, (s, [intToAtom iv]))
                    1 -> Just (guid, (s, [boolToAtom (if iv == 1 then True else False)]))
                    2 -> Just (guid, (s, [stringToAtom sv]))
                    3 -> Just (guid, (s, [GrzAtomFile iv]))
        Nothing -> Nothing
    where
        guid = (fromSql objectGuid) :: Int
        sv = (fromSql stringValue) :: String
        iv = (fromSql integerValue) :: Int
