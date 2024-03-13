--
-- GSIの「基準点成果等閲覧サービス」から取得できる三角点リストを加工する
--
module Main where

import Debug.Trace

import Data.List
import System.Environment
import Text.XML.Light

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

---        x -> trace ("not name" ++ show x) p

    getCoord :: Element -> (Double, Double)
    getCoord (Element { elContent = c }) =
      let CData {cdData = s} = (onlyText c) !! 0
          (lat, (',':lon)) = break ((==) ',') s
      in
        (read lat, read lon)
        
{-
Element {elName = QName {qName = "Placemark", qURI = Just "http://www.opengis.net/kml/2.2", qPrefix = Nothing},
         elAttribs = [],
         elContent = [Elem (Element {elName = QName {qName = "name", qURI = Just "http://www.opengis.net/kml/2.2", qPrefix = Nothing},
                                     elAttribs = [],
                                     elContent = [Text (CData {cdVerbatim = CDataText, cdData = "TR15439010501", cdLine = Just 26})],
                                     elLine = Just 26}),
                      Elem (Element {elName = QName {qName = "styleUrl", qURI = Just "http://www.opengis.net/kml/2.2", qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "#t1p", cdLine = Just 26})], elLine = Just 26}),
                      Elem (Element {elName = QName {qName = "Point", qURI = Just "http://www.opengis.net/kml/2.2", qPrefix = Nothing},
                                     elAttribs = [],
                                     elContent = [Elem (Element {elName = QName {qName = "coordinates", qURI = Just "http://www.opengis.net/kml/2.2", qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "139.1900705,36.00575167", cdLine = Just 26})], elLine = Just 26})], elLine = Just 26}),
                      Elem (Element {
                               elName = QName {qName = "description",
                                                qURI = Just "http://www.opengis.net/kml/2.2",
                                                qPrefix = Nothing},
                                 elAttribs = [],
                                 elContent = [Text (CData {cdVerbatim = CDataVerbatim, cdData = "  <div style=\"font-size:1em;\"><table border=1 cellspacing=0><tr><th>\19968\31561\19977\35282\28857</th><th>\22530\24179\23665</th></tr><tr><th></th><th>875.91 m</th></tr><tr><th colspan=2>36.00575167 139.1900705</th></tr><tr><th>2014-04-01</th><th>2999-12-31</th></tr></table></div>  ", cdLine = Just 26})],
                                      elLine = Just 26})],
         elLine = Just 26}

[Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\22522\28310\28857\12467\12540\12489", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "TR15439055601", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1}),
 Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\31561\32026\31278\21029", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "TR1", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1}),
 Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\22522\28310\28857\21517", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\39640\37326\26449", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\12527\12531\12473\12488\12483\12503\21487\21542", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\9675", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\25104\26524\29366\24907", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\27491\24120", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\21271\32239", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "36\176\&02\8242\&45\8243.3633", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\26481\32076", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "139\176\&42\8242\&01\8243.5379", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\27161\39640", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "10.15", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\29694\27841\29366\24907", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\27491\24120", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\24180\26376\26085", cdLine = Just 1})], elLine = Just 1}),Elem (Element {elName = QName {qName = "td", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "20110622", cdLine = Just 1})], elLine = Just 1})], elLine = Just 1})]
-}


--
-- Local Variables:
-- coding: utf-8
-- End:
