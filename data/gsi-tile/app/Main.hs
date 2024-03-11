module Main where

-- import Debug.Trace
import Control.Monad (when, unless)
--import Data.Bits
import Data.Either.Combinators
import Data.Foldable (foldlM, forM_)
import Data.Maybe (catMaybes)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Process (callCommand)
import Text.Printf
import Text.Read (readMaybe)
import Text.XML.Light

import GeoAngle
import GsiTile

data CommandLineOpt =
    ShowUrlOnly
  | MarkPoint
  | OutputImageFile String
  | UseElev | UseOcr
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
  -- , Option ['P'] ["find-peak"] (NoArg UseElev) "find a peak using elevation data"
  , Option ['?']["help"] (NoArg Help) "show help"
  ]

data ProgramOptions = ProgramOptions {
    optOutputImageFile :: Maybe String
  , optUrlOnly :: Bool
  , optMark :: Bool
  , optPeak :: PeakSearchingMethod
  , optHelp :: Bool
  } deriving Show

getOptions :: [String] -> Either String (ProgramOptions, [String])
getOptions args = do
  (opts, a) <- case getOpt RequireOrder options args of
    (f,a,[]) -> return (f,a)
    (_,_,err) -> Left (concat err)
  o <- foldlM checkOption (ProgramOptions Nothing False False NoMethod False) opts

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
                   UseElev -> return $ opts { optPeak = ElevData }
                   
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
        -- putStrLn $ show opts
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
    doTile opts tile coord = do
      unless (optUrlOnly opts) (printInfo tile coord)
      printUrls (optUrlOnly opts) tile

      when (optOutputImageFile opts == Nothing && optMark opts)
           (hPutStrLn stderr "ignoring --mark as --output is not specified")

      if optPeak opts == NoMethod
        then case (optOutputImageFile opts, optMark opts) of
               (Nothing, _) -> return ()
               (Just outputfile, mark) -> getTileImage tile coord outputfile opts
        else findPeak tile coord opts

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

getTileImage :: TileInfo -> Coordinate -> String -> ProgramOptions -> IO ()
getTileImage tile coord outputfile opts = do
  let tileimg = if (optMark opts) then "tmp.png" else outputfile
  -- XXX escaspe special characters in outputfile and url
  downloadTileImage tile 0 0 tileimg
  when (optMark opts) $ do
    callCommand $ printf "pngtopam '%s' | ppmdraw -script='%s' | pamtopng > '%s'" tileimg makeScript outputfile
  
  where
    makeScript :: String
    makeScript = makeScriptToMarkCoord tile coord "blue"
  

downloadTileImage :: TileInfo -> Int -> Int -> String -> IO ()
downloadTileImage tile xspanidx yspanidx filename =
  callCommand (printf "wget -O '%s' '%s'" filename (url tile xspanidx yspanidx))


data DetectedString = DetectedString {
    hpos :: Int
  , vpos :: Int
  , width :: Int
  , height :: Int
  , txt :: String
  } deriving (Show)

findPeak :: TileInfo -> Coordinate -> ProgramOptions -> IO ()
findPeak tile coord opts = do
  let tileimg = case (optMark opts, optOutputImageFile opts) of
                  (False, Nothing) -> "tmp.png"
                  (False, Just f) -> f
                  (True, _) -> "tmp.png"
  downloadTileImage tile 0 0 tileimg
  callCommand $ printf "pngtopam %s > tmp2.ppm" tileimg
  callCommand "ppmchange black black -remainder=white tmp2.ppm | pamtopng > tmp2.png"
  callCommand  "tesseract tmp2.png tmp3 --psm 11 alto"
  strings <- readFile "tmp3.xml" >>=  pure . getStrings . parseXML
  if null strings
    then hPutStrLn stderr "can't find any peak"
    else do
      let peak = peakCoord $ head strings
      putStrLn $ printf "Peak=%s" (show peak)
      case (optMark opts, optOutputImageFile opts) of
        (True, Just output) -> do
          let script = makeScriptToMarkCoord tile coord "blue"
                       ++ "setcolor brown;"
                       ++ (concat $ map makeScriptToBox strings)
                       ++ makeScriptToMarkCoord tile peak "red"
          callCommand $ printf "ppmdraw --script='%s' tmp2.ppm | pamtopng > '%s'" script output
        (_, _) -> return ()

  where
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

    peakCoord ds =
      let x = hpos ds
          y = (vpos ds) + (height ds) `div` 2
          west = toDouble (lonWest tile)
          w = toDouble (lonEast tile) - west
          north = toDouble (latNorth tile)
          h = north - toDouble (latSouth tile)
      in
        Coordinate (Double_ (west + w * (fromIntegral x) / (fromIntegral (xpixels tile)))) (Double_ (north - h * (fromIntegral y) / (fromIntegral (ypixels tile))))
          
  
makeScriptToMarkCoord :: TileInfo -> Coordinate -> String -> String
makeScriptToMarkCoord tile coord color =
  let sz = 20
      (x, y) = getOffset tile coord
      limitx _x = max 0 $ min _x (xpixels tile - 1)
      limity _y = max 0 $ min _y ((ypixels tile) - 1)
  in
    printf "setcolor %s; line %d %d %d %d; line %d %d %d %d"
          color
          (limitx (x - sz)) y
          (limitx (x + sz)) y
          x (limity (y - sz))
          x (limity (y + sz))

makeScriptToBox :: DetectedString -> String
makeScriptToBox s =
      concat [printf "setpos %d %d;" (hpos s) (vpos s),
              printf "line_here %d 0;" (width s),
              printf "line_here 0 %d;" (height s),
              printf "line_here -%d 0;"(width s),
              printf "line_here 0 -%d;" (height s)]
