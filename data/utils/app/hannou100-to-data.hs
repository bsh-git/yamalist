{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Text as T
import Data.Text.IO as TIO
import GHC.Generics
import System.Environment
import System.Exit
import System.IO
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

import Debug.Trace

data Geometry = Geometry
  { coordinates:: [Float]
  } deriving (Show, Generic)

instance FromJSON Geometry

data Properties = Properties
  { number:: Integer
  , name:: Text
  , altitude:: Text
  , latitude:: Text
  , longitude:: Text
  , url1:: Maybe Text
  , url2:: Maybe Text
  } deriving  (Show, Generic)

instance FromJSON Properties

data Feature = Feature
  { geometry:: Geometry
  , properties:: Properties
  } deriving (Show, Generic)

instance FromJSON Feature

data GeoJson = GeoJson { features:: [Feature] } deriving (Show, Generic)
instance FromJSON GeoJson


generate :: GeoJson -> IO ()
generate geoj = do
  TIO.putStrLn $ T.intercalate (T.pack "\t") [listIdHannou100, "L", T.pack "飯能百名山"]
  generate' (features geoj)

generate' :: [Feature] -> IO ()
generate' [] = return ()
generate' (x:xs) = generate1 x >> generate' xs

generate1 :: Feature -> IO ()
generate1 (Feature {geometry=geom, properties=prop})  = do
  let id = T.pack $ show $ number prop
  TIO.putStrLn $
    T.intercalate (T.pack "\t") [
          id
        , T.pack "D"
        , longitude prop
        , latitude prop
        , altitude prop
        , T.pack "-"    -- prefecture
        , urlToYamarecoID $ url1 prop
        , urlToYamapID $ url2 prop
        , T.pack "-"
        , T.pack "-"  -- source
        , T.pack "-"  -- peak correction
        ]
  case splitAlias (name prop) of
    Nothing -> putNameLine id "N" $ name prop
    Just (name1, name2) -> do
      putNameLine id "N1" name1
      putNameLine id "N2" name2

putNameLine:: Text -> String -> Text -> IO ()
putNameLine id mark name = TIO.putStr id >> System.IO.putStr ("\t" ++ mark ++ "\t") >> TIO.putStrLn name

splitAlias :: Text -> Maybe (Text, Text)
splitAlias name =
  let (pre, _, _, a) = name =~ (T.pack "\\((.+)\\)") :: (Text, Text, Text, [Text])
  in
    if Prelude.length a == 0 then Nothing
    else Just (pre, a !! 0)

listIdHannou100 = T.pack "1110"

yamareco_url_regex = T.pack "https://www.yamareco.com/modules/yamainfo/ptinfo.php\\?ptid=([0-9]+)"

urlToYamarecoID :: Maybe Text -> Text
urlToYamarecoID Nothing = T.pack "-"
urlToYamarecoID (Just txt) =
  let (_, _, _, a) =  txt =~ yamareco_url_regex :: (Text, Text, Text, [Text])
  in
    a !! 0

yamap_url_regex = T.pack "https://yamap.com/([a-z]+)/([0-9]+)"

urlToYamapID :: (Maybe Text) -> Text
urlToYamapID Nothing = T.pack "-\t-"
urlToYamapID (Just txt) =
  let (_, _, _, a) = txt =~ yamap_url_regex :: (Text, Text, Text, [Text])
  in
    makeYamapIDFields a

makeYamapIDFields [] = T.pack "-\t-"
makeYamapIDFields (cat:id:_) =
  T.pack $ yamapPointType cat ++ "\t" ++ (T.unpack id)

yamapPointType :: Text -> String
yamapPointType ct =
  case ct of
    "mountains" -> "1"
    "landmarks" -> "2"
    _ -> "3"

convert :: Handle -> IO ()
convert h = do
  json <- BL.hGetContents h
  case Data.Aeson.decode json :: Maybe GeoJson of
--  case Data.Aeson.decode json :: Maybe Properties of
    Nothing -> System.Exit.die "parse error"
    Just g -> generate g

convertFile :: String -> IO ()
convertFile filename =
  openFile filename ReadMode >>= convert

main :: IO ()
main = do
  a <- getArgs
  case Prelude.length a of
    0 -> convert stdin
    1 -> convertFile $ a !! 0
    _ -> System.IO.hPutStrLn stderr "too many arguments"

{-
  case Data.Aeson.decode json :: Maybe Geometry of
    Nothing -> print "Parse error"
    Just x -> print x
-}
