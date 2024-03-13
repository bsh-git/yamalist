module GeoAngle ( Angle(..)
                , toDouble
                , integralPart
                , minutePart
                , secondPart
                , Coordinate(..)
                , latitude
                , longitude
                ) where

import GHC.Read
import Text.ParserCombinators.ReadPrec ()
import Text.Printf
import qualified Text.Read.Lex as L
import Text.Read

--
-- 経度, 緯度
--
data Coordinate = Coordinate Angle Angle deriving Show

latitude :: Coordinate -> Angle
latitude (Coordinate _ lat) = lat
longitude :: Coordinate -> Angle
longitude (Coordinate lon _) = lon

--
-- 経度,緯度に使う角度
--
data Angle =
    Double_ Double
  | Tuple_ Int Int Double  -- 度,分,秒
  deriving Eq

-- |
-- >>> toDouble (Double_ 123.4)
-- 123.4
-- >>> toDouble (Tuple_ 135 30 3.6)
-- 135.501
toDouble :: Angle -> Double
toDouble (Double_ f) = f
toDouble (Tuple_ i m s) | i >= 0 = (fromIntegral i) + (fromIntegral m) / 60.0 + s / 3600.0
toDouble (Tuple_ i m s) =  - ( (fromIntegral (- i)) + (fromIntegral m) / 60.0 + s / 3600.0)

{-
-- |
-- >>> asDouble (Double_ 123.4)
-- (Double_ 123.4)
-- >>> asDouble (Tuple_ 135 30 3.6)
-- (Double_ 135.501)
asDouble :: Angle -> Angle
asDouble f@(Double_ _) = f
asDouble x@(Tuple_ _ _ _) = Double_ (toDouble x)
-}

asTuple :: Angle -> Angle
asTuple t@(Tuple_ _ _ _) = t
asTuple (Double_ f) | f >= 0 =
  let i = floor f
      r = 60 * (f - (fromIntegral i))
      m = floor r
      r2 = 60 * (r - (fromIntegral m))
  in
    Tuple_ i m r2

asTuple (Double_ f) =
  let (Tuple_ i m s) = asTuple (Double_ (- f))
  in
    Tuple_ (- i) m s

instance Show Angle where
  show (Tuple_ d m s)  = (show d) ++ "°" ++ (show m) ++ "'" ++ (printf "%.4f" s) ++ "\""
  show a = show $ asTuple a

--
-- BUG!!: 整数部分が0の時に負号がなくなってしまう
--
negate :: Angle -> Angle
negate (Double_ f) = (Double_ (- f))
negate (Tuple_ i m s) = (Tuple_ (- i) m s)

-- |
-- 角度の整数部分(「度]の部分)
--
-- >>> integralPart (Tuple_ 135 2 3.5)
-- 135
-- >>> integralPart (Double_ (- 135.123))
-- -135
integralPart:: Angle -> Int
integralPart (Tuple_ i _ _) = i
integralPart (Double_ f) | f >= 0 = floor f
integralPart (Double_ f)          = - ( floor (- f))

-- |
-- >>> minutePart (Tuple_ 135 2 3.5)
-- 2
-- >>> minutePart (Double_ 135.501)
-- 30
minutePart:: Angle -> Int
minutePart (Tuple_ _ m _) = m
minutePart x@(Double_ _) = minutePart $ asTuple x

-- |
-- >>> secondPart (Tuple_ 135 2 3.5)
-- 3.5
-- >>> Text.Printf.printf "%.6f" $ secondPart (Double_ 135.501)
-- 3.600000
secondPart:: Angle -> Double
secondPart (Tuple_ _ _ s) = s
secondPart x@(Double_ _) = secondPart $ asTuple x

-- |
-- 文字列からの変換
-- 経度・緯度の書式
--    nn度mm分ss.sss秒
--    nn°mm'ss.sss"
--    nn:mm:ss.sss
-- 「度」のかわりに「°」か「:」も使える
-- 「分」のかわりに「'」か「:」も使える
-- 「秒」のかわりに「"」も使える
-- 「秒」は省略可
--
-- TODO: 国土地理院のページで見られる nn°nn'ss".sss  の表記に対応する
--
-- >>> (read "45.1234") == Double_ 45.1234
-- True
-- >>> (read "45度12分34.56秒") == Tuple_ 45 12 34.56
-- True
-- >>> (read "45°12'34.56\"") == Tuple_ 45 12 34.56
-- True
--
-- 秒のマークは省略可
-- >>> (read "45:12:34.56") == Tuple_ 45 12 34.56
-- True
--
-- 混ぜても良い。
-- >>> (read "45°12分34.56") == Tuple_ 45 12 34.56
-- True
--
-- 数字に0を前置してよい
-- >>> (read "45°02分04.56") == Tuple_ 45 2 4.56
-- True
--
-- 秒の小数点はあってもなくてもよい
-- >>> (read "45°02分04") == Tuple_ 45 2 4
-- True
--
-- 秒以外には小数点を書けない
-- >>> (readMaybe "45°2.0分04秒") :: Maybe Angle
-- Nothing
-- >>> (readMaybe "45°2分4.5秒") == Just (Tuple_ 45 2 4.5)
-- True

instance Read Angle where
  readPrec = do
    sgn <- look
    case sgn of
      ('-':_) -> do
        (L.Symbol "-") <- lexP
        d <- readPrec'
        return (GeoAngle.negate d)
      _ -> readPrec'

    where
      readPrec' = readPrecTuple <++ readPrecDouble 
        
  readListPrec = readListPrecDefault

readPrecDouble :: ReadPrec Angle
readPrecDouble = do
  x <- lexP
  case x of
    (L.Number n) ->  return $ Double_ (fromRational (L.numberToRational n))
    _ -> pfail

readPrecTuple :: ReadPrec Angle
readPrecTuple = do
  (L.Number i) <- lexP
  i' <- checkInteger i
  expectChar ":°度"
  (L.Number m) <- lexP
  m' <- checkInteger m
  expectChar ":'分"
  (L.Number s) <- lexP
  optionalChar "秒\""

  return (Tuple_ i' m' (fromRational (L.numberToRational s)))

  where
    checkInteger n =
      case L.numberToInteger n of
        Just i -> return (fromIntegral i)
        _ -> pfail

expectChar :: [Char] -> ReadPrec ()
expectChar s = do
  c <- get
  if c `elem` s
    then return ()
    else pfail
    
optionalChar :: [Char] -> ReadPrec ()
optionalChar s = do
  rest <- look
  case rest of
    "" -> return ()
    (c:_) -> if c `elem` s
               then do
                 _ <- get
                 return ()
              else
                return ()
