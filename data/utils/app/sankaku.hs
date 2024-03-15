--
-- GSIの「基準点成果等閲覧サービス」から取得できる三角点リストを加工する
--
module Main where

import Data.List
import System.Environment
import Text.XML.Light

--import Debug.Trace


main :: IO ()
main =
  getArgs >>= mapM_ processFile

  where
    processFile kmlfile = do
      xml <- readFile kmlfile >>= pure . parseXML

      let Just doc = find (\e -> qName (elName e) == "kml") (onlyElems xml)
                     >>= filterChild (\e -> qName (elName e) == "Document")

      let placemarks = filterChildrenName (\qn -> qName qn == "Placemark") doc

      mapM_ (outputPlacemark . makePlacemark) placemarks


    outputPlacemark :: Placemark -> IO ()
    outputPlacemark (Placemark n (lon, lat) _ desc) =
      putStrLn $ intercalate "\t" [n, show lon, show lat, desc]
                       
data Placemark = Placemark { pmName :: String
                           , pmCoord :: (Double, Double)
                           , pmElev :: Float
                           , pmDesc :: String
                           } deriving Show


makePlacemark :: Element -> Placemark
makePlacemark mark =
  foldl buildPlacemark (Placemark "" (0.0, 0.0) 0.0 "") $  onlyElems (elContent mark)

  where
    buildPlacemark :: Placemark -> Element -> Placemark
    buildPlacemark p e =
      case e of
        Element {elName = QName {qName = "name"}, elContent = cnt} ->
          let CData {cdData = s} = (onlyText cnt) !! 0
          in
            p { pmName = s }
        Element {elName = QName {qName = "Point"}, elContent = cnt} ->
          let coord = getCoord ((onlyElems cnt) !! 0)
          in
            p { pmCoord = coord }

        Element {elName = QName {qName = "description"}, elContent = cnt} ->
          let CData { cdData = s } = (onlyText cnt) !! 0
              Element {elContent = tbl} = (onlyElems (parseXML s)) !! 0
              Element {elContent = tr} = (onlyElems tbl) !! 0
              Element {elContent = td} = (onlyElems tr) !! 1
              CData {cdData = code} = (onlyText td) !! 0
          in
            p { pmDesc = code }

        _ -> p

    getCoord :: Element -> (Double, Double)
    getCoord (Element { elContent = c }) =
      let CData {cdData = s} = (onlyText c) !! 0
          (lat, (',':lon)) = break ((==) ',') s
      in
        (read lat, read lon)
        
--
-- Local Variables:
-- coding: utf-8
-- End:
