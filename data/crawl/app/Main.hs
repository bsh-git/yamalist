{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.Foldable (foldlM)
import Data.List
import Data.List.Extra (snoc, (!?))
import Data.Maybe (isJust, maybeToList, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as ICUC
import Data.Tree
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Temp
import System.Process (callCommand)
import Text.HTML.Parser
import Text.HTML.Tree
import Text.Printf
import Text.Read
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

import Debug.Trace
--trace :: String -> a -> a
--trace _ e = e

ePutStrLn = hPutStrLn stderr

data CommandLineOpt =
    YamarecoList String
  | SaveHtml FilePath
  | FromFile FilePath
  | Help
  deriving (Show)

options :: [OptDescr CommandLineOpt]
options =
  [ Option ['L']["list"] (ReqArg YamarecoList "LIST-ID") "get a list of mountains (ex. 百名山) from yamareco"
  , Option ['S']["save-html"] (ReqArg SaveHtml "FILENAME") "save html from yamareco"
  , Option ['F']["from-file"] (ReqArg FromFile "FILENAME") "use local file"
  , Option ['?']["help"] (NoArg Help) "show help"
  ]

data ProgramOptions = ProgramOptions
  { optSaveHtml :: Maybe FilePath
  , optListToGet :: Maybe Int
  , optFromFile :: Maybe FilePath
  , optHelp :: Bool
  } deriving Show

getOptions :: [String] -> Either String (ProgramOptions, [String])
getOptions args = do
  (opts, a) <- case getOpt RequireOrder options args of
    (f,a,[]) -> return (f,a)
    (_,_,err) -> Left (concat err)
  o <- foldlM checkOption (ProgramOptions Nothing Nothing Nothing False) opts

  return (o, a)

  where
    checkOption :: ProgramOptions -> CommandLineOpt -> Either String ProgramOptions
    checkOption opts input =
      return =<< case input of
                   Help -> return $ opts { optHelp = True }
                   SaveHtml f -> return $ opts { optSaveHtml = Just f }
                   FromFile f -> return $ opts { optFromFile = Just f }
                   YamarecoList id -> if isJust (optListToGet opts) then Left "too manu -L options"
                                      else return opts { optListToGet = Just (read id) }

main :: IO ()
main = getArgs >>= (either optError main') . getOptions
  where
    usage = getProgName >>= \p -> return $ "Usage: " ++ usageInfo (p ++ " [options]") options
    optError msg = do
      usage >>= ePutStrLn
      die msg

    main' (opts, args) = do
      if optHelp opts
        then usage >>= putStrLn >> exitSuccess
        else
          withSystemTempDirectory "tmpdir" $ main'' opts args
        
    main'' opts _ tmpdir =
      mapM_ (getList opts tmpdir) $ maybeToList (optListToGet opts)

getList :: ProgramOptions -> FilePath -> Int -> IO ()
getList opts tmpdir listId = do
  converter <- ICUC.open "EUC-JP" Nothing
  tags <- case (optSaveHtml opts, optFromFile opts) of
    (_, Just input) -> parseFile converter input
    (Nothing, Nothing) -> readList' converter (printf "%s/%d.html" tmpdir listId)
    (Just output, Nothing) -> readList' converter output

  let tags' = clean tags
  
  let (ptlist, _) = getBlock "ul" $ dropWhile (not . isPtlistMapSelect) tags'
  let Right [tree] = tokensToForest ptlist
--  putStrLn $ show $ (head forest)
  let coordList = map makeCoord $ subForest tree

  let (table, _) = getBlock "table" tags'
  let Right forest = (tokensToForest table)
  let tbody = (subForest (forest !! 0)) !! 1
  ---let Right forest = tokensToForest table
  ---mapM_ (putStrLn . show) (subForest tbody)
  let d = convertTbody tbody
  mapM_ (\r -> case r of
                 (ColText number:mnt@ColMountainName {mtName=name, mtYomi=yomi, mtId=id}:ColText elev:_) ->
                     outputResult coordList number mnt elev
--                   putStrLn $ intercalate "\t" (map T.unpack [number, yomi, elev, link])
                 _ -> putStrLn $ "???" ++ (show r)
                 ) d

  where
    parseFile converter file = B.readFile file >>= pure . parseTokens . (ICUC.toUnicode converter)

    clean = removeScripts . removeWhite

    readList' converter output = do
      callCommand $ printf "wget -nv -O '%s' '%s'" output (listIdToUrl listId)
      parseFile converter output

    convertTbody :: Tree Token -> [[ColumnContent]]
    convertTbody tbody = map convertTr (subForest tbody)

    convertTr :: Tree Token -> [ColumnContent]
    convertTr forest = map convertTd (subForest forest)

    outputResult coordList number (ColMountainName name yomi ptid) elev' = do
      let num = (read (T.unpack number)) :: Int
      let elev = elevToFloat elev'
      let (lons, lats) =
            case coordList !? (num - 1) of
              Just (lon, lat) -> (show lon, show lat)
              _ -> ("-", "-")
      putStrLn $ intercalate "\t" [show num, "D", lons, lats, show elev, (maybe "" show ptid), "?", "?", "-", "r"]
      outputNames num name yomi

    outputNames :: Int -> T.Text -> T.Text -> IO ()
    outputNames num name yomi =
      if not (name =~ name_alias_regex)
      then
        putStrLn $ intercalate "\t" [show num, "N", T.unpack name, T.unpack yomi]
      else
        let (prev,_,_,grp) = name =~ name_alias_regex :: (T.Text,T.Text,T.Text,[T.Text])
            name2 = grp !! 0
        in
          if name2 =~ T.pack "P[0-9]"
            then
              let name' = T.unpack prev ++ "(" ++ T.unpack name2 ++ ")"
              in putStrLn $ intercalate "\t" [show num, "N", name', T.unpack yomi]
            else do
              putStrLn $ intercalate "\t" [show num, "N", T.unpack prev, T.unpack yomi]
              putStrLn $ intercalate "\t" [show num, "N", T.unpack name2, "-"]

    name_alias_regex = T.pack "[(（](.+)[）)]"

    elevToFloat s' =
      let s = T.unpack s'
      in
        case elemIndex 'm' s of
          Just idx -> read (take idx s) :: Float
          Nothing -> read s :: Float

    isPtlistMapSelect :: Token -> Bool
    isPtlistMapSelect tag = getAttr "class" tag == Just "ptlist_map_select"
      
    makeCoord :: Tree Token -> (Double, Double)
    makeCoord Node {rootLabel = TagOpen "li" _, subForest = s} = makeCoord (head s)
    makeCoord Node {rootLabel = tag@(TagOpen "a" atrs)} =
      let lon = fromMaybe 0.0 $ getAttr "data-lon" tag >>= \s -> readMaybe (T.unpack s)
          lat = fromMaybe 0.0 $ getAttr "data-lat" tag >>= \s -> readMaybe (T.unpack s)
      in
        (lon, lat)
    makeCoord _ = undefined

data ColumnContent = ColText T.Text
  | ColLink { linkText :: T.Text, linkUrl :: T.Text }
  | ColMountainName { mtName :: T.Text, mtYomi ::T.Text, mtId:: Maybe Int }
  | ColBad ColumnContent [String]
  deriving Show

convertTd :: Tree Token -> ColumnContent
convertTd td = foldl checkTd (ColText "") $ subForest td

  where
    checkTd :: ColumnContent -> Tree Token -> ColumnContent
    checkTd (ColBad cc s) node = ColBad cc (s ++ [(show node)])
    checkTd accum (Node {rootLabel=Comment _}) = accum
    checkTd accum (Node {rootLabel=TagOpen "div" [Attr "class" "dropdown"]}) = accum
    checkTd (ColText s0) (Node {rootLabel=ContentText s1}) = ColText (T.append s0 s1)
    checkTd accum (Node {rootLabel=ContentText s}) = ColBad accum [T.unpack s]

    checkTd accum node@(Node {rootLabel = TagOpen "a" attrs, subForest=f}) =
      case accum of
        ColText "" -> case getLink attrs f of
                        Just (t, l) -> ColLink t l
                        Nothing -> ColBad accum [show node]
        _ -> ColBad accum [show node]

    checkTd accum@(ColLink txt url) (Node {rootLabel=TagOpen "span" [Attr "class" "f-sm"], subForest=f}) =
      ColMountainName { mtName = txt, mtYomi = makeYomi f, mtId = urlToId url }
    checkTd accum node = ColBad accum [show node]

    ptid_url_regex = T.pack "ptinfo.php\\?ptid=([0-9]+)"

    urlToId :: T.Text -> Maybe Int
    urlToId url =
      case url =~ ptid_url_regex :: (T.Text,T.Text,T.Text,[T.Text]) of
        (_,_,_,(s:ss)) -> readMaybe (T.unpack s)
        _ -> Nothing

    makeYomi f = removeEnclosing (fromMaybe "" (getText f))  '（' '）'

removeEnclosing :: T.Text -> Char -> Char -> T.Text
removeEnclosing txt beg end =
  let txt' = case T.unsnoc txt of
               Just (txt', e) | e == end -> txt'
               _ -> txt
  in
    case T.uncons txt' of
      Just (b, txt'') | b == beg -> txt''
      _ -> txt'


getLink :: [Attr] -> [Tree Token] -> Maybe (T.Text, T.Text)
getLink attrs forest = do
  href <- findHref attrs
  txt <- getText forest
  return (txt, href)

getText forest = foldl accumText (Just "") forest
  where
    accumText Nothing _ = Nothing
    accumText (Just t) (Node {rootLabel=ContentText s}) = Just (T.append t s)
    accuText (Just t) _ = Just (T.append t " ??? ")

findHref :: [Attr] -> Maybe T.Text
findHref (Attr "href" url:_) = Just url
findHref (_:as) = findHref as
findHref _ = Nothing


listIdToUrl :: Int -> String
listIdToUrl listId = "https://www.yamareco.com/modules/yamainfo/ptlist.php?groupid=" ++ (show listId)

removeScripts :: [Token] -> [Token]
removeScripts tokens =
  snd $ foldl remove (False, []) tokens
  where
    remove :: (Bool, [Token]) -> Token -> (Bool, [Token])
    remove (inScript, out) token =
      case token of
        TagOpen "script" _ -> (True, out)
        TagClose "script" -> (False, out)
        TagSelfClose "script" _ -> undefined
        _ -> if inScript then (True, out) else (False, out ++ [token])

removeWhite :: [Token] -> [Token]
removeWhite = filter (\t -> case t of
                         ContentText s | all (\c -> c `elem` whitespaces) (T.unpack s) -> False
                         _ -> True)
  where whitespaces = " \t\r\n" :: String

-- |
--
-- >>> getAttr "class" (TagOpen "xxx" [Attr "id" "foo", Attr "class" "bar"])
-- Just "bar"
-- >>> getAttr "class" (TagOpen "xxx" [Attr "id" "foo", Attr "xxx" "yyy"])
-- Nothing
-- >>> getAttr "class" (TagClose "xxx")
-- Nothing
getAttr :: T.Text -> Token -> Maybe T.Text
getAttr aName (TagOpen _ attrs) =
  find (\a -> let Attr n _ = a in n == aName) attrs >>= \(Attr _ v) -> Just v
getAttr _ _ = Nothing

getBlock0 :: T.Text -> [Token] -> [Token]
getBlock0 name tokens = snd $ foldl getBalanced (0, []) tokens
  where
    getBalanced (lvl, out) token =
      case token of
        TagOpen s _ | s == name -> (lvl + 1, out ++ [token])
        TagClose s | s == name -> (lvl - 1, out ++ [token])
        _ -> if lvl == 0 then (lvl, out) else (lvl, out ++ [token])


getBlock :: T.Text -> [Token] -> ([Token], [Token])
getBlock name tokens =
  let (_, list, rest) = getBlock' (0, []) tokens
  in
    (list, rest)

  where
    getBlock' :: (Int, [Token]) -> [Token] -> (Int, [Token], [Token])
    getBlock' (lvl, out) [] = (lvl, out, [])
    getBlock' (lvl, out) (token:rest) =
      case token of
        (TagOpen s _) | s == name -> getBlock' (lvl + 1, out ++ [token]) rest
        (TagClose s) | s == name -> if lvl == 1 then (0, out ++ [token], rest)
                                  else getBlock' ((lvl - 1), out ++ [token]) rest
        _ -> if lvl > 0 then getBlock' (lvl, out ++ [token]) rest
                 else getBlock' (lvl, out) rest


testdata = [
 TagOpen "div" [Attr "style" "margin-top: 5px;",Attr "class" "ptlist_map_select"],
 TagOpen "div" [],
 ContentText "\22320\22259\19978\12395\34920\31034",
 TagClose "div",
 TagOpen "ul" [Attr "class" "list-unstyled fs-13"],
 TagOpen "li" [],
 TagOpen "a" [Attr "data-lon" "138.841278",Attr "data-lat" "36.023444",Attr "class" "maplist",Attr "href" "javascript:void(0);"],
 ContentText "\20001\31070\23665",
 TagClose "a",
 TagClose "div",
 TagOpen "xxxx" []
 ]
