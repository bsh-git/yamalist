{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Control.Monad (when, unless, forM) -- void
--import Data.Bits
--import Data.Char (isDigit)
import Control.Applicative.HT (lift2)
import Data.Either.Combinators
import Data.Foldable (foldlM, forM_)
import Data.Functor ((<&>))
import Data.List
import Data.List.Extra ((!?), snoc)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Word
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import qualified Data.ByteString.Lazy as B
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
--import System.Process (callCommand)
import Text.Printf
import Text.Read (readMaybe, readEither)

import GeoAngle
import GsiTile
import YamaData

import Debug.Trace
--trace :: String -> a -> a
--trace _ e = e

ePutStrLn = hPutStrLn stderr
--ePrint = ePutStrLn . show
traceDump s x = trace (s +=+ show x) x
(+=+) :: String -> String -> String
a +=+ b = a ++ " " ++ b
infixr 5 +=+

data CommandLineOpt =
    COMasterFile !FilePath
  | COOutputFile !FilePath
  | COHelp
  deriving (Show)

options :: [OptDescr CommandLineOpt]
options =
  [ Option ['M']["master"] (ReqArg COMasterFile "MASTER") "name of the master file"
  , Option ['o']["output"] (ReqArg COOutputFile "OUTPUT") "name of the file to create"
  , Option ['?']["help"] (NoArg COHelp) "show help"
  ]

data ProgramOptions = ProgramOptions
  { optMasterFile :: !(Maybe FilePath)
  , optOutputFile :: !(Maybe FilePath)
  , optHelp :: !Bool
  } deriving Show

getOptions :: [String] -> Either String (ProgramOptions, [String])
getOptions args = do
  (opts, a) <- case getOpt RequireOrder options args of
    (f,a,[]) -> return (f,a)
    (_,_,err) -> Left (concat err)
  o <- foldlM checkOption (ProgramOptions Nothing Nothing False) opts

  return (o, a)

  where
    checkOption :: ProgramOptions -> CommandLineOpt -> Either String ProgramOptions
    checkOption opts input =
      case input of
        COHelp -> return $ opts { optHelp = True }
        COMasterFile s -> return $ opts { optMasterFile = Just s }

-- リストソースファイル全体に対応するデータ
data ListSource = ListSource { listId :: !(Maybe ListId)
                             , listName :: !T.Text
                             , listInfo :: [T.Text]
                             , listSummits :: [(IndexInList, PointData)] }
                  deriving Show



main :: IO ()
main = getArgs >>= either optError main' . getOptions
  where
    usage progName =
      "Usage: " ++ usageInfo (progName ++ " [OPTIONS] ListSource...") options
    optError msg = do
      progName <- getProgName
      ePutStrLn $ progName ++ ": error: " ++ msg ++ "\n"
                  ++ usage progName
      exitFailure

    main' (opts, args) =
      if optHelp opts then do
        getProgName >>= \p -> putStrLn $ usage p
        exitSuccess
      else do
        master <- case optMasterFile opts of
                    Nothing -> return masterDbInit
                    Just mfile -> readMaster mfile

        master' <- foldlM processListSource master args
        outputMaster stdout master'


    processListSource :: MasterDatabase -> FilePath -> IO MasterDatabase
    processListSource master listFile = do
      -- let outputFile =  listFile -<.> "tsv.NEW"
      src <- readDataSource listFile


      let mklist = case listId src of
                     Nothing -> const []
                     Just lid -> (\sid -> [(lid, sid)])

      let src' = src { listSummits =
                         map (\(sid,s)
                               -> (sid, s { pointLists = mklist sid }))
                             (reverse (listSummits src)) }

      let (master', msgs) = foldl' updateMaster (master, []) (listSummits src')
      mapM_ ePutStrLn msgs

      return $ maybe master' (updateListName master' (listName src')) (listId src')

      where
        updateMaster (m,report) (rid, pd) =
          let (m',pid,ss) = mergePoint m (rid, pd) in (m', report ++ ss)

        updateListName m name lid =
          m { mdLists = M.adjust (setListName name) lid (mdLists m)}

outputMaster :: Handle -> MasterDatabase -> IO ()
outputMaster h master = do
  forM_ (M.assocs (mdSummits master)) $
        \(pid, pd) -> do
          hPutStrLn h $ intercalate "\t" [ show pid
                                         , "D"
                                         , show (pointLongitude pd)
                                         , show (pointLatitude pd)
                                         , show (pointElevation pd)
                                         , showPref (pointPrefecture pd)
                                         , showYamareco (pointYamareco pd)
                                         , showYamap (pointYamap pd)
                                         , showLists (pointLists pd)
                                         , T.unpack (pointSource pd)
                                         , T.unpack (pointPeakCorrection pd)
                                         ]
          outputNames pid $ pointNames pd

  mapM_ (\(lid,ld) ->printf "%d\tL\t%s\n" lid (ldName ld))  (M.assocs (mdLists master))


  where
    showPref prefs = if null prefs then "-" else intercalate "," $ map show prefs
    showYamareco = maybe "-" show
    showYamap y = case y of
                    Nothing -> "-\t-"
                    Just (YamapLocation yid) -> "location\t" ++ show yid
                    Just (YamapMountain yid) -> "mountain\t" ++ show yid
    showLists ls = intercalate "," $ map (\(lid,idx) -> show lid ++ ":" ++ show idx) ls

    outputNames ptid = mapM_ (outputName ptid)
    outputName ptid name =
      hPutStrLn h $ intercalate "\t" $
          [show ptid, "N", T.unpack (locName name)] ++ map T.unpack (locYomi name)


-- リストソースファイルの各行に対応するデータ
data ListSourceEntry = LineList ListId T.Text [T.Text]    -- L
                     | LineSummit IndexInList PointData      -- D
                     | LineName Int T.Text [T.Text]       -- N
                     | LineError
                     deriving Show


readMaster :: FilePath -> IO MasterDatabase
readMaster inputFile = do
  (points, listNames) <- readTsv True inputFile
  return $ foldl' setListName (fromList points) listNames

  where
    setListName:: MasterDatabase -> (ListId, T.Text, a) -> MasterDatabase
    setListName master (id, name, _) =
      master { mdLists = case M.lookup id (mdLists master) of
                           Nothing -> M.insert id (ListDef name []) (mdLists master)
                           Just (ListDef _ points) ->
                             M.insert id (ListDef name points) (mdLists master)}

readDataSource :: FilePath -> IO ListSource
readDataSource inputFile = do
  (points, listNames) <- readTsv False inputFile

  case listNames of
    [] -> return $ ListSource Nothing "" [] points -- not for a list but points
    [(lid, name, rest)] -> return $ ListSource (Just lid) name rest points
    _toomanyLrecords -> fail (printf "multiple L-records in %s. aborting" inputFile)

readTsv :: Bool -> FilePath -> IO ([(Int, PointData)],[(ListId, T.Text, [T.Text])])
readTsv isMaster inputFile = do
  contents <- B.readFile inputFile <&> decodeUtf8

  mapM (\l -> case processLine l of
                Right e -> return e
                Left msg -> ePutStrLn ("Error: " ++ msg) >> return LineError
         )
       (T.lines contents)
    <&> foldl' foldEntry ([], [])

  where
    processLine :: T.Text -> Either String ListSourceEntry
    processLine line =
      case T.split (=='\t') line of
            (num:"L":name:rest) -> readEitherT' num >>= \n -> Right $ LineList n name rest
            (num:typ:name:yomi) | 'N' == T.head typ ->
                                  readEitherT' num >>= \n -> Right $ LineName n name yomi
            (num:"D":rest) -> dRecord num rest
            lineWithOtherMarks -> Left $ "Unknown data line: " ++ T.unpack line

    dRecord num fields = do
      sid <- readEitherT' num :: Either String Int

      if isMaster && sid < 10000
        then Left $ printf "Bad ID (%s) in master file %s" num inputFile
        else if not isMaster && sid >= 10000
             then Left $ printf "Index (%s) is too big in list source file %s" num inputFile
             else dRecord' sid fields

    dRecord' sid fields = do
      lon <- readAngle (fields !? 0)
      lat <- readAngle (fields !? 1)
      elev <- readElev (fields !? 2)
      pref <- readPref (fields !? 3)
      yamareco <- readId (fields !? 4)
      yamap <- readYamapPtId fields
      lists <- if isMaster then readLists (fields !? 7) else pure []
      source <- txtField (fields !? 8)
      peakcorrection <- txtField (fields !? 9)

      return $ LineSummit sid $
        PointData lon lat elev [] pref yamareco yamap lists source peakcorrection

    readPref Nothing = Right []
    readPref (Just "-") = Right []
    readPref (Just t) = readEither $ "[" ++ T.unpack t ++ "]"

    readElev Nothing = missing "elevation"
    readElev (Just "-") = missing "elevation"
    readElev (Just e) = readEitherT' e

    readId Nothing = Right Nothing
    readId (Just "-") = Right Nothing
    readId (Just n) = readEitherT' n >>= \id -> Right (Just id)
--      either Left (\id -> Right (Just id)) $ readEitherT' n

    readYamapPtId _ = Right Nothing

    txtField Nothing = Right "-"
    txtField (Just s) = Right s

    readAngle Nothing = Left "missing angle"
    readAngle (Just s) = readEitherT' s

    readLists :: Maybe T.Text -> Either String [(ListId, IndexInList)]
    readLists Nothing = Right []
    readLists (Just s) = mapM readList' $ T.split (==',') s
    readList' s = case T.split (==':') s of
                    [listId_, idx_] -> lift2 (,) (readEitherT' listId_) (readEitherT' idx_)
                    _badformat -> Left $ printf "incorrect format %s" s

    missing :: String -> Either String a
    missing s = Left $ "missing " ++ s

    readEitherT' :: Read a => T.Text -> Either String a
    readEitherT' s = case readEitherT s of
                       Right a -> Right a
                       Left msg -> Left $ msg ++ " : " ++ T.unpack s

    readEitherT :: Read a => T.Text -> Either String a
    readEitherT = readEither . T.unpack

    readT :: Read a => T.Text -> a
    readT = read . T.unpack

    foldEntry accum LineError = accum -- ignore the line.
    foldEntry (pds, lds) (LineList lid name rest) = (pds, (lid,name,rest):lds)
    foldEntry (pds, lds) (LineSummit pid pd) = ((pid,pd):pds, lds)
    foldEntry (pds, lds) line@(LineName {}) = (addName pds line,lds)

    addName [] line = error $ printf "Bad N-record (%s) in %s" (show line) inputFile
    addName ((pid, pd):pds) (LineName pid' name yomi)
      | pid /= pid' = error $ printf "Id mismatch for D-record and N-record (%s:%s)in %s" pid pid'
      | otherwise =
        let n = pointNames pd
            pd' = pd { pointNames = snoc n (LocName name yomi) }
        in (pid, pd'):pds


--
-- Local Variables:
-- coding: utf-8
-- End:
