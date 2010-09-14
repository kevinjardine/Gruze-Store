module Data.Store.Gruze.Query

where

-- internal functions to convert and run query definitions

import Data.Store.Gruze.Types
import Data.Store.Gruze.Container
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
addToQuery :: Maybe GrzQuery 
    -> String 
    -> Maybe GrzQuery
addToQuery q extra = 
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
           
queryRowToObj :: Map.Map Int GrzAtomBox -> [SqlValue] -> GrzObj
queryRowToObj m [oid,ot,otc,otu,oo,oc,os,e] =
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
            
-- TODO: get this to work with GROUPed results
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

joinToString :: GrzQueryDefItem -> [String]
joinToString (GrzQDJoin x) = [x]
joinToString _ = []

getQueryJoins :: [GrzQueryDefItem] -> [String]
getQueryJoins q = concatMap joinToString q

whereToString :: GrzQueryDefItem -> [String]
whereToString (GrzQDWhere s) = [s]
whereToString _ = []

getQueryWheres :: [GrzQueryDefItem] -> [String]
getQueryWheres q = concatMap whereToString q

getQueryType :: [GrzQueryDefItem] -> GrzQueryType
getQueryType q = case concatMap isQueryType q of
                        [] -> GrzQTFull
                        (x:xs) -> x
isQueryType :: GrzQueryDefItem -> [GrzQueryType]
isQueryType (GrzQDType t) = [t]
isQueryType _ = []


-- Query IO functions
       
grzCreateQuery :: GrzHandle -> (GrzQueryDef -> GrzQueryDef) -> IO (Maybe GrzQuery)
grzCreateQuery grzH q = grzGetQuery grzH (q (0, []))

-- derives the query string, looking up any string handles in the database
-- if any string handle lookups fail, this function returns Nothing
-- otherwise it returns the list of need names and the actual query string
grzGetQuery :: GrzHandle -> GrzQueryDef -> IO (Maybe GrzQuery)
grzGetQuery grzH (_, q) =
    do     
        prefix <- readFile $ (grzDataDirectory grzH) ++ "/config/mysql/" ++ "prefix" ++ sqlFn
        suffix <- readFile $ (grzDataDirectory grzH) ++ "/config/mysql/" ++ "suffix" ++ sqlFn
        maybeFrags <- grzQueryWhereFragments grzH q
        if isNothing maybeFrags
            then
                return Nothing
            else do
                let justFrags = fromJust maybeFrags
                let qv = map snd justFrags
                let d = concatMap (snd . fst) justFrags
                let frags = map (fst . fst) justFrags
                grzLog grzH ("frags: " ++ (show frags))
                grzLog grzH ("whereBit: " ++ (show whereBit))
                return $ Just (
                    (((prefix
                    ++ " "
                    ++ (intercalate " " (getQueryJoins q))
                    ++ (if (null whereBit) && (null frags)
                            then ""
                            else (" WHERE " 
                                ++ (intercalate " AND " whereBit)
                                ++ (if (null frags) || (null whereBit) then "" else " AND ")
                                ++ (intercalate " AND " frags))
                        )
                    ++ " " 
                    ++ suffix), queryType), d),
                   (needs, (concatMap fst qv, concatMap snd qv)) )
    where
        whereBit = getQueryWheres q
        needs = getQueryNeeds q
        queryType = getQueryType q
        sqlFn = case queryType of
                    GrzQTCount ->  "_query_count.sql"
                    GrzQTID ->  "_query_guid.sql"
                    GrzQTFull -> "_query_full.sql"
                    GrzQTAggCount -> "_query_agg_count.sql"
                    GrzQTAggSumCount -> "_query_agg_sumcount.sql"
                   
grzQueryWhereFragments :: GrzHandle 
    -> [GrzQueryDefItem] 
    -> IO (Maybe [((String,[(String,Int)]), GrzQueryValues)])
grzQueryWhereFragments grzH q = do
    r <- mapM (grzHandleWhereQueryFragment grzH) frags
    if and $ map isJust r
        then
            return $ Just (map fromJust r)
        else
            return Nothing
    where
        frags = filter (\x -> case x of
                                GrzQDWhereFrags _ -> True
                                otherwise -> False
                       ) q

grzHandleWhereQueryFragment :: GrzHandle 
    -> GrzQueryDefItem 
    -> IO (Maybe ((String,[(String,Int)]), GrzQueryValues))
grzHandleWhereQueryFragment grzH (GrzQDWhereFrags items) = do
    r <- mapM (qwfItemToMaybeString grzH) items
    if and $ map isJust r
        then do
            let j = map fromJust r
            let s = concatMap (fst . fst) j
            let d = concatMap (snd . fst) j
            let v = map snd j            
            return $ Just ((s,d), (concatMap fst v, concatMap snd v))
        else
            return Nothing

qwfItemToMaybeString :: GrzHandle 
    -> GrzQDWFItem 
    -> IO (Maybe ((String,[(String,Int)]), GrzQueryValues))  
qwfItemToMaybeString grzH item =
    case item of
        GrzQDString s -> return $ Just ((s,[]),([],[]))
        GrzQDName s -> do
                        grzLog grzH ("Processing GrzQDName " ++ s)
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
