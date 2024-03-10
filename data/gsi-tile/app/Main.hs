module Main where

-- import Debug.Trace
import Control.Monad (when, unless)
--import Data.Bits
import Data.Either.Combinators
import Data.Foldable (foldlM)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Process (callCommand)
import Text.Printf
import Text.Read (readMaybe)

import GeoAngle
import GsiTile

data CommandLineOpt =
    ShowUrlOnly
  | MarkPoint
  | OutputImageFile String
  | Help
  deriving (Show)

options :: [OptDescr CommandLineOpt]
options =
  [ Option ['O']["output"] (ReqArg OutputImageFile "OUTPUT")
           "download the tile and save to OUTPUT"
  , Option ['U']["url"] (NoArg ShowUrlOnly) "print only URL"
  , Option []["mark"] (NoArg MarkPoint) "add a mark in the tile"
  , Option ['?']["help"] (NoArg Help) "show help"
  ]

data ProgramOptions = ProgramOptions {
    optOutputImageFile :: Maybe String
  , optUrlOnly :: Bool
  , optMark :: Bool
  , optHelp :: Bool
  } deriving Show

getOptions :: [String] -> Either String (ProgramOptions, [String])
getOptions args = do
  (opts, a) <- case getOpt RequireOrder options args of
    (f,a,[]) -> return (f,a)
    (_,_,err) -> Left (concat err)
  o <- foldlM checkOption (ProgramOptions Nothing False False False) opts

  return (o, a)


  where
    checkOption :: ProgramOptions -> CommandLineOpt -> Either String ProgramOptions
    checkOption opts input =
      return =<< case input of
                   Help -> return $ opts { optHelp = True }
                   ShowUrlOnly -> return $ opts { optUrlOnly = True }
                   MarkPoint -> return $ opts { optMark = True }
                   OutputImageFile s -> return $ opts { optOutputImageFile = Just s }

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
      case (optOutputImageFile opts, optMark opts) of
        (Nothing, False) -> return ()
        (Nothing, True) -> hPutStrLn stderr "ignoring --mark as --output is not specified"
        (Just outputfile, mark) -> getTileImage tile coord outputfile mark

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

getTileImage :: TileInfo -> Coordinate -> String -> Bool -> IO ()
getTileImage tile coord outputfile mark = do
  let ofile = if mark then "tmp.png" else outputfile
  -- XXX escaspe special characters in outputfile and url
  callCommand (printf "wget -O '%s' '%s'" ofile (url tile 0 0))
  when mark $ do
    callCommand $ printf "pngtopam '%s' | ppmdraw -script='%s' | pamtopng > '%s'" ofile makeScript outputfile
  
  where
    makeScript :: String
    makeScript =
      let sz = 20
          (x, y) = getOffset tile coord
          limitx _x = max 0 $ min _x (xpixels tile - 1)
          limity _y = max 0 $ min _y ((ypixels tile) - 1)
      in
        printf "setcolor blue; line %d %d %d %d; line %d %d %d %d"
          (limitx (x - sz)) y
          (limitx (x + sz)) y
          x (limity (y - sz))
          x (limity (y + sz))
  
