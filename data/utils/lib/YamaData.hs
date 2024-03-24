{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module YamaData ( ListId
                , LocName(..)
                , MasterDatabase (..)
                , masterDbInit
                , fromList
                , mergePoint
                , PointData (..)
                , PointId
                , primaryName
                , YamarecoPtId
                , YamapPtId (..)
                , ListDef (..)
                , IndexInList
                , setListName
                ) where

--import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy
import Data.List
import Data.List.Extra (snoc)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text.Lazy.Encoding ()
import Data.Word
import Text.Printf
import Text.Read (readMaybe, readEither)

import GeoAngle

import Debug.Trace
--trace :: String -> a -> a
--trace _ e = e
--ePutStrLn = hPutStrLn stderr
traceDump s x = trace (s +=+ show x) x

(+=+) :: String -> String -> String
a +=+ b = a ++ " " ++ b


-- |
-- 山頂などの名前
-- 読みは複数あるかもしれない
--
data LocName = LocName { locName :: T.Text
                       , locYomi :: [T.Text]
                       } deriving (Show, Eq)


-- |
-- リストのID
--
type ListId = Word16
type IndexInList = Int

-- |
-- YamarecoでのID
type YamarecoPtId = Word16

-- |
-- YamapでのID
data YamapPtId = YamapLocation Int | YamapMountain Int deriving (Show, Eq)

-- |
-- 山頂データのID
type PointId = Int

--getIdAsInt :: PointId -> Int
--getIdAsInt (PointId gid) = gid
--getIdAsInt (LocalSummitId sid) = sid

-- |
-- 山頂データ (峠,小屋など山頂以外にも使うかもしれないが今は山頂のみ)
--
data PointData = PointData
  { pointLongitude :: !Angle
  , pointLatitude :: !Angle
  , pointElevation :: !Float
  , pointNames :: ![LocName]
  , pointPrefecture :: ![Word8]
  , pointYamareco :: !(Maybe YamarecoPtId)
  , pointYamap :: !(Maybe YamapPtId)
  , pointLists :: [(ListId, IndexInList)]
  , pointSource :: !T.Text
  , pointPeakCorrection :: !T.Text
  }
  deriving (Show, Eq)

getCoordinate :: PointData -> (Angle, Angle)
getCoordinate dat = (pointLongitude dat, pointLatitude dat)

primaryName :: PointData -> Maybe T.Text
primaryName pd = case pointNames pd of
                   [] -> Nothing
                   (n:_) -> Just (locName n)

type PointMap = M.Map PointId PointData

-- |
-- マスターデータベース
-- 山頂、リストなど全てを保持する
-- (将来的には、NoSQLなどを使うかもしれないが、今はオンメモリに全部持つ)
--
data MasterDatabase = MasterDatabase
  {
    mdSummits :: PointMap
  , mdLists :: M.Map ListId ListDef
  , mdMaxIdAssigned :: !PointId
  } deriving (Show, Eq)

data ListDef = ListDef
               { ldName :: !T.Text
               , ldPoints :: ![(IndexInList, PointId)]
               }
               deriving (Show, Eq)

setListName :: T.Text -> ListDef -> ListDef
setListName name ld = ld { ldName = name }

masterDbInit :: MasterDatabase
masterDbInit =
  MasterDatabase M.empty M.empty 9999

assignPointId :: MasterDatabase -> (PointId, MasterDatabase)
assignPointId db = let newpid = 1 + mdMaxIdAssigned db
                   in (newpid, db { mdMaxIdAssigned = newpid })


fromList :: [(PointId, PointData)] -> MasterDatabase
fromList pds = foldl addToMasterDb masterDbInit pds

addToMasterDb :: MasterDatabase -> (PointId, PointData) -> MasterDatabase
addToMasterDb db (pid, pd) =
  db { mdSummits = updatePointMap (mdSummits db) pid pd
     , mdLists = updateListMap (mdLists db) (pointLists pd)
     , mdMaxIdAssigned = mdMaxIdAssigned db `max` pid
     }

  where
    updateListMap:: M.Map ListId ListDef -> [(ListId, IndexInList)] -> M.Map ListId ListDef
    updateListMap = foldl' updateListMap'
    updateListMap' m (lid, idx) =
        case M.lookup lid m of
          Nothing -> M.insert lid (ListDef "" [(idx, pid)]) m
          Just (ListDef name lst) -> M.insert lid (ListDef name ((idx,pid):lst)) m

updatePointMap :: PointMap -> PointId -> PointData -> PointMap
updatePointMap sm pid pd = M.insert pid pd sm

mergePoint :: MasterDatabase ->  (IndexInList, PointData) -> (MasterDatabase, PointId, [String])
mergePoint master (rid, newData) =
  case findPointByLocation master (getCoordinate newData) of
    Nothing ->
      -- it's new summit
      let (pid, db) = assignPointId master
       in (addToMasterDb db (pid, newData), pid, [])
    Just (pid, existing) ->
      -- already registered or very close to it
      if existing == newData
        then (master, pid, [])
        else
          let (newpd, report0) = mergePointData existing newData
           in ( master {mdSummits = M.insert pid newpd (mdSummits master)},
                pid,
                report0 ++ makeDiffReport existing newData
              )

locEpsilon :: Angle
locEpsilon = fromSecond 2.0

findPointByLocation :: MasterDatabase -> (Angle, Angle) -> Maybe (PointId, PointData)
findPointByLocation master (lon, lat) =
  find (isCloseEnough . snd) $ M.assocs (mdSummits master)

  where isCloseEnough pd =
          let (lon', lat') = getCoordinate pd
          in
            abs (lon - lon') < locEpsilon &&
            abs (lat - lat') < locEpsilon

{--
        traceout pd (lon', lat') =
          "isCLoseEnough:" +=+ show pd +=+ show (lon,lat)
          +=+ show (lon', lat') +=+ show (abs (lon - lon')) +=+ show (abs (lat - lat'))
          +=+ "epsilon=" +=+ show locEpsilon
-}

makeDiffReport :: PointData -> PointData -> [String]
makeDiffReport pd1 pd2 = []

mergePointData :: PointData -> PointData -> (PointData, [String])
mergePointData pd1 pd2 =
  foldl' runMerge (pd1, []) [mergeLink, mergeNames]

  where
    runMerge (pd, report) f =
      let (pd', report') = f pd
      in (pd', report ++ report')

    mergeLink pd =
      let l0 = pointLists pd1
          (msg, newl) =  checkLinks $ sortOn fst $ l0 ++ pointLists pd2
      in
        if l0 == newl
        then (pd, [])
        else
          (pd { pointLists = newl },
           printf "link updated for %s" (fromMaybe "?" $ primaryName pd) : msg)

    -- remove duplicates in link list and warn conflicts
    checkLinks = checkLinks' [] []
    checkLinks' msg accum [] = (msg, accum)
    checkLinks' msg accum [a] = (msg, accum ++ [a])
    checkLinks' msg accum (a:b:cs) | a == b = checkLinks' msg (accum ++ [a]) (b:cs)
    checkLinks' msg accum (a:b:cs) | fst a == fst b =
                                     checkLinks' (msg ++ ["conflicts on link xxx for yyy"])
                                                 accum (b:cs)
    checkLinks' msg accum (a:as) = checkLinks' msg (accum ++ [a]) as

    mergeNames :: PointData -> (PointData, [String])
    mergeNames pd =
      let (report, newNames) = mergeNames' (IntMap.fromList $ zip [0..] (pointNames pd)) (pointNames pd2)
      in
        (pd { pointNames = IntMap.elems newNames }, report)

    mergeNames' names1 names2 = foldl' mergeName ([], names1) names2
    mergeName (reports, accum) n =
      case find (\m -> locName (snd m) == locName n) (IntMap.assocs accum) of
        Nothing -> -- simply add a new name
          (snoc reports "add new name",
            addNewName accum n)
        Just (idx, v) -> -- possibly update yomi
          let v' = v {locYomi = nub $ locYomi v ++ locYomi n}
          in
            if v' == v then (reports, accum)  -- no update
            else (snoc reports "add to yomi", IntMap.insert idx v' accum)

    addNewName accum new =
      let (mx, _) =IntMap.findMax accum
      in
        IntMap.insert (mx + 1) new accum

{-
haveSameName :: PointData -> PointData -> Bool
haveSameName x y = haveSameName' (pointNames x) (pointNames y)
  where
    haveSameName' a b =
      any (\(a',b') -> locName a' == locName b') $ concatMap (\na -> map (na,) b) a
-}
