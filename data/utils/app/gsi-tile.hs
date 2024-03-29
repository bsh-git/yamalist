module Main where

import Control.Monad (when, unless, forM) -- void
--import Data.Bits
import Data.Char (isDigit)
import Data.Either.Combinators
import Data.Foldable (foldlM)
import Data.List
import Data.Maybe (catMaybes)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Temp
import System.Process (callCommand)
import Text.Printf
import Text.Read (readMaybe)
import Text.XML.Light

import GeoAngle
import GsiTile

import Debug.Trace
--trace :: String -> a -> a
--trace _ e = e

data CommandLineOpt =
    ShowUrlOnly
  | MarkPoint
  | OutputImageFile String
  | UseElev | UseOcr
  | XSpan String
  | KeepOcrResult
  | Help
  deriving (Show)

data PeakSearchingMethod = NoMethod | Ocr | ElevData deriving (Show, Eq)


options :: [OptDescr CommandLineOpt]
options =
  [ Option ['O']["output"] (ReqArg OutputImageFile "OUTPUT")
           "download the tile and save to OUTPUT"
  , Option ['U']["url"] (NoArg ShowUrlOnly) "print only URL"
  , Option []["mark"] (NoArg MarkPoint) "add a mark in the tile"
  , Option ['T'] ["find-peak-ocr"] (NoArg UseOcr) "find a peak using OCR on the tile"
  , Option ['K'] ["keep-ocr-result"] (NoArg KeepOcrResult) "keep output from OCR as OUTPUT with extension .xml"
  -- , Option ['P'] ["find-peak"] (NoArg UseElev) "find a peak using elevation data"
  , Option ['x']["xspan"] (ReqArg XSpan "N") "concatnate N tiles hollizontally"
  , Option ['?']["help"] (NoArg Help) "show help"
  ]

data ProgramOptions = ProgramOptions {
    optOutputImageFile :: Maybe String
  , optUrlOnly :: Bool
  , optMark :: Bool
  , optPeak :: PeakSearchingMethod
  , optXSpan :: Int
  , optKeepOcr :: Bool
  , optHelp :: Bool
  } deriving Show

getOptions :: [String] -> Either String (ProgramOptions, [String])
getOptions args = do
  (opts, a) <- case getOpt RequireOrder options args of
    (f,a,[]) -> return (f,a)
    (_,_,err) -> Left (concat err)
  o <- foldlM checkOption (ProgramOptions Nothing False False NoMethod 1 False False) opts

  return (o, a)


  where
    checkOption :: ProgramOptions -> CommandLineOpt -> Either String ProgramOptions
    checkOption opts input =
      return =<< case input of
                   Help -> return $ opts { optHelp = True }
                   ShowUrlOnly -> return $ opts { optUrlOnly = True }
                   MarkPoint -> return $ opts { optMark = True }
                   OutputImageFile s -> return $ opts { optOutputImageFile = Just s }
                   UseOcr -> return $ opts { optPeak = Ocr }
                   KeepOcrResult -> return $ opts { optKeepOcr = True }
                   UseElev -> return $ opts { optPeak = ElevData }
                   XSpan n -> return $ opts { optXSpan = (read n) }

main :: IO ()
main = getArgs >>= (either optError main') . getOptions
  where
    main' (opts, args) =
      if optHelp opts then do
        usage >>= putStrLn
        exitSuccess
      else if length args /= 2 then
        usage >>= hPutStrLn stderr >> die "too few or too many arguments"
      else do
        -- ePutStrLn show opts
        case getTile args of
          Right (tile, c) -> doTile opts tile c
          Left msg -> die msg

    usage = getProgName >>= \p -> return $ usageInfo (p ++ " [options] LONGITUDE LATITUDE") options
    optError msg = do
      usage >>= hPutStrLn stderr
      die msg

    getTile :: [String] -> Either String (TileInfo, Coordinate)
    getTile args = do
      c <- argsToCoord (args!!0) (args!!1)
      tile <- tileForCoordinate 17 c
      Right (tile, c)


    argsToCoord lonstr latstr = do
      lon <- conv lonstr
      lat <- conv latstr
      Right $ Coordinate lon lat

      where conv s = maybeToRight
                       ("bad string for longitude/latitude: " ++ s)
                       (readMaybe s :: Maybe Angle)

    doTile :: ProgramOptions -> TileInfo -> Coordinate -> IO ()
    doTile opts tile' coord = do
      let tile = case (optXSpan opts) of
            1 -> tile'
            2 -> extendToEast tile'
            _ -> undefined

      unless (optUrlOnly opts) (printInfo tile coord)
      printUrls (optUrlOnly opts) tile

      when (optOutputImageFile opts == Nothing && optMark opts)
           (hPutStrLn stderr "ignoring --mark as --output is not specified")

      if optPeak opts == NoMethod
        then case (optOutputImageFile opts, optMark opts) of
               (Nothing, _) -> return ()
               (Just outputfile, _) -> withTmp $ getTileImage tile coord outputfile opts
        else withTmp $ findPeak tile coord opts

    printInfo :: TileInfo -> Coordinate -> IO ()
    printInfo tile coord =
      let (xoff, yoff) = getOffset tile coord
      in
        printf "North=%s\nSouth=%s\nWest=%s\nEast=%s\nWidth=%d\nHeight=%d\nXspan=%d\nYspan=%d\nOffsetX=%d\nOffsetY=%d\n"
               (show (latNorth tile)) (show (latSouth tile)) (show (lonWest tile)) (show (lonEast tile))
               (xpixels tile) (ypixels tile)
               (xspan tile) (yspan tile)
               xoff yoff

    printUrls flag tile =
      let prnt = if flag then printUrl else printkv
          printkv x y url_ = printf "url_%d_%d=%s\n" x y url_
          printUrl _ _ url_ = url_
      in
        putStr $ concatMap (\(x,y) -> prnt x y (url tile x y))
                           [(x,y) | x <- [0 .. (xspan tile) -1], y <- [0 .. (yspan tile) -1]]

    withTmp = withSystemTempDirectory "tmpdir"

getTileImage :: TileInfo -> Coordinate -> String -> ProgramOptions -> FilePath -> IO ()
getTileImage tile coord outputfile opts tmpdir = do
  let tileimg = if (optMark opts) then (printf "%s/tmp.ppm" tmpdir) else outputfile
  -- XXX escaspe special characters in outputfile and url
  downloadTileImage tile tmpdir tileimg

  when (optMark opts) $ do
    callCommand $ printf "ppmdraw -script='%s' %s %s > '%s'"
                         makeScript tileimg
                         (if ".ppm" `isSuffixOf` outputfile then "" else " | pamtopng")
                         outputfile

  where
    makeScript :: String
    makeScript = makeScriptToMarkCoord tile coord "blue"

downloadSingleTile :: TileInfo -> Int -> Int -> String -> IO ()
downloadSingleTile tile xspanidx yspanidx filename =
  if ".ppm" `isSuffixOf` filename
    then callCommand $ printf "wget -nv -O - '%s' | pngtopam > '%s'" (url tile xspanidx yspanidx) filename
    else callCommand $ printf "wget -nv -O '%s' '%s'" filename (url tile xspanidx yspanidx)

downloadTileImage :: TileInfo -> FilePath -> FilePath -> IO ()
downloadTileImage tile tmpdir output = do
  hPutStrLn stderr (printf "downloadTIleImage: xspan=%d yspan=%d tmpdir=%s output=%s" (xspan tile) (yspan tile) tmpdir output)
  case (xspan tile, yspan tile) of
    (1, 1) -> downloadSingleTile tile 0 0 output
    (_, 1) -> makeRowImage tile tmpdir output 0
    (_, ys) -> do
      files <- forM [0..ys-1] $ \y -> do
        let rowfile = printf "%s/row%d.ppm" tmpdir y
        makeRowImage tile tmpdir rowfile y
        return rowfile
      concatVertical output files

  where
    concatVertical :: FilePath -> [FilePath] -> IO ()
    concatVertical output' files = do
      let prog = (++) "pamcat -tb " $ intercalate " " $ map (printf "'%s'") files
      callCommand $ (if ".ppm" `isSuffixOf` output' then prog else prog ++ "| pamtopng") ++ " > " ++ output

makeRowImage :: TileInfo -> FilePath -> FilePath -> Int -> IO ()
makeRowImage tile tmpdir output yidx_ = do
  hPutStrLn stderr (printf "makeRowImage: tmpdir=%s output=%s" tmpdir output)
  let toPpm = ".ppm" `isSuffixOf` output
  files <- forM [0 .. (xspan tile)-1] $ \xidx_ -> do
    let tmpname = printf "%s/%04d" tmpdir xidx_
    downloadSingleTile tile xidx_ yidx_ (tmpname ++ ".png")
    let out = tmpname ++ ".ppm"
    callCommand (printf "pngtopam '%s.png' > '%s'" tmpname out )
    return out
  let prog = (++) "pamcat -lr " $ intercalate " " $ map (printf "'%s'") files
  callCommand $ (if toPpm then prog else prog ++ "| pamtopng") ++ " > " ++ output

data DetectedString = DetectedString {
    hpos :: Int
  , vpos :: Int
  , width :: Int
  , height :: Int
  , txt :: String
  } deriving (Show)

--
-- 国土地理院地図の標高表記に対してtesseract OCRすると、 -NNNN や +NNNN と認識される (黒ドットが -や+ と認識される)
-- 三角点がある場合は、ANNNN
--
isElevationText :: DetectedString -> Bool
isElevationText ds = isElevationText' (txt ds)
  where
    isElevationText' (c:rest) | c `elem` "A-+*"  = isElevationText'' rest
    isElevationText' s = isElevationText'' s
    isElevationText'' = all (\c -> c `elem` ".," || isDigit c)

findPeak :: TileInfo -> Coordinate -> ProgramOptions -> FilePath -> IO ()
findPeak tile_ coord opts tmpdir = do
  let (x', y') = getOffset tile_ coord
  let tile = mayExtendVertical y' $ mayExtendHorizontal x' tile_
  let (x, y) = getOffset tile coord

  downloadTileImage tile tmpdir (tmpfile 1 "ppm")
  callCommand $ printf "ppmchange black black -remainder=white '%s' | pamtopng > '%s'" (tmpfile 1 "ppm") (tmpfile 2 "png")

  let ocrout = case ((optKeepOcr opts), (optOutputImageFile opts)) of
        (False, _) -> (tmpfile 3 "")
        (_, Nothing) -> (tmpfile 3 "")
        (True, Just f) -> dropExtension f

  callCommand $ printf "tesseract '%s' '%s' --psm 11 alto" (tmpfile 2 "png") ocrout

  strings <- readFile (ocrout ++ ".xml") >>=  pure . getStrings . parseXML
  let elvstrings = selectElevationText strings

  script <- case chooseBestElvString (x, y) elvstrings of
    Nothing -> do
      hPutStrLn stderr "can't find any peak"
      pure $ makeScriptToMarkCoord tile coord "blue"
    Just elvstr -> do
      let peak = peakCoord tile elvstr
      putStrLn $ printf "Peak=%s,%s" (show (longitude peak)) (show (latitude peak))
      pure $ makeScriptToMarkCoord tile coord "blue"
             ++ (concat $ map makeScriptToBox strings)
             ++ makeScriptToMarkCoord tile peak "red"

  case (optMark opts, optOutputImageFile opts) of
    (True, Just output) -> callCommand $ printf "ppmdraw -script='%s' '%s' | pamtopng > '%s'" script (tmpfile 1 "ppm") output
    (False, Just output) -> callCommand $ printf "pamtopng '%s' > '%s'" (tmpfile 1 "ppm") output
    (_, _) -> return ()

  where
    tmpfile :: Int -> String -> String
    tmpfile n "" = printf "%s/%d" tmpdir n
    tmpfile n ext = printf "%s/%d.%s" tmpdir n ext

    getStrings :: [Content] -> [DetectedString]
    getStrings content = catMaybes $ map conv strings
      where
        alto = (onlyElems content) !! 1
        strings = filterElementsName (\qn -> ((qName qn) == "String")) alto
        conv :: Element -> Maybe DetectedString
        conv elm = do
          s <- sequence $ map (\name ->  findAttrBy (\qn -> qName qn == name) elm ) ["HPOS", "VPOS", "WIDTH", "HEIGHT"]
          str <- findAttrBy (\qn -> qName qn == "CONTENT") elm
          let d = map read s :: [Int]
          return $ DetectedString (d!!0) (d!!1) (d!!2) (d!!3) str

    peakCoord tile ds =
      let (x, y) = adjustOffset ds (hpos ds, (vpos ds) + (height ds) `div` 2)
          west = toDouble (lonWest tile)
          w = toDouble (lonEast tile) - west
          north = toDouble (latNorth tile)
          h = north - toDouble (latSouth tile)
      in
        Coordinate
          (fromDouble (west + w * (fromIntegral x) / (fromIntegral (xpixels tile))))
          (fromDouble (north - h * (fromIntegral y) / (fromIntegral (ypixels tile))))

    dropExtension :: String -> String
    dropExtension name =
      let pos = elemIndices '.' name in
        if null pos then name
        else take (last pos) name

    selectElevationText :: [DetectedString] ->[DetectedString]
    selectElevationText = filter isElevationText

    adjustOffset ds (x, y) =
      case (txt ds) of
        ('A':_) -> (x + 10, y)
        _       -> (x+2, y+2)

    mayExtendHorizontal :: Int -> TileInfo -> TileInfo
    mayExtendHorizontal x tile | x >= ((xpixels tile) `div` 2) = extendToEast tile
    mayExtendHorizontal x tile | x < (xpixels tile) * 10 `div` 100  = extendToWest tile
    mayExtendHorizontal _ tile = tile

    mayExtendVertical y tile | y <  (ypixels tile) * 10 `div` 100 = extendToNorth tile
    mayExtendVertical y tile | y >  (ypixels tile) * 90 `div` 100 = extendToSouth tile
    mayExtendVertical _ tile = tile

    chooseBestElvString _ elvs | null elvs = Nothing
    chooseBestElvString origpos elvs =
      check $ foldl choose (Nothing, 99999) elvs

      where
        choose :: (Maybe DetectedString, Int) -> DetectedString -> (Maybe DetectedString, Int)
        choose (e, distance) elv =
          let x = hpos elv - fst origpos
              y = vpos elv - snd origpos
              d = x * x + y + y
          in
            trace (show (hpos elv, fst origpos, hpos elv - fst origpos, vpos elv, snd origpos, vpos elv - snd origpos, "distance", d))
                  (if (d < distance) then (Just elv, d) else (e, distance))

        check (Nothing, _) = Nothing
        check (Just _, d) | d >= 5000 = (trace "too far" Nothing)
        check (Just e, _) = Just e

makeScriptToMarkCoord :: TileInfo -> Coordinate -> String -> String
makeScriptToMarkCoord tile coord color =
  let sz = 20
      (x, y) = getOffset tile coord
      limitx _x = max 0 $ min _x (xpixels tile - 1)
      limity _y = max 0 $ min _y ((ypixels tile) - 1)
  in
    printf "setcolor %s; line %d %d %d %d; line %d %d %d %d;"
          color
          (limitx (x - sz)) y
          (limitx (x + sz)) y
          x (limity (y - sz))
          x (limity (y + sz))

makeScriptToBox :: DetectedString -> String
makeScriptToBox s =
      concat [printf "setpos %d %d;" (hpos s) (vpos s),
              printf "setcolor %s;" (if isElevationText s then "green" else "brown"),
              printf "line_here %d 0;" (width s),
              printf "line_here 0 %d;" (height s),
              printf "line_here -%d 0;"(width s),
              printf "line_here 0 -%d;" (height s)]


ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr
