module GeoAngle ( Angle
                , fromIntegralAngle
--                , fromRationalAngle
                , fromSecond
                , fromDouble
                , toDouble
                , integerPart
                , minutePart
                , secondPart
                , Coordinate(..)
                , latitude
                , longitude
                , asDouble
                , asTuple
                ) where

import Data.Word
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
  | Tuple_ Bool Word8 Word8 Double  -- 負号,度,分,秒

-- |
-- >>> fromIntegralAngle 123
-- 123°0'0.0000"
-- >>> fromIntegralAngle (-85)
-- -85°0'0.0000"
fromIntegralAngle:: (Integral a) => a -> Angle
fromIntegralAngle n = if n >= 0
                      then Tuple_ False (fromIntegral n) 0 0
                      else Tuple_ True (- (fromIntegral n)) 0 0

-- |
-- >>> fromSecond 30
-- 0°0'30.0000"
-- >>> fromSecond (-30)
-- -0°0'30.0000"
-- >>> asTuple $ fromSecond 120
-- 0°2'0.0000"
fromSecond :: Double -> Angle
fromSecond s | s < 0 = negate ( fromSecond (- s))
fromSecond s | s < 60.0 = Tuple_ False 0 0 ( s)
fromSecond s = Double_ (s / 3600.0)

-- |
-- >>> fromDouble 123.4
-- 123.4
fromDouble:: Double -> Angle
fromDouble f = (Double_ f)

-- |
-- >>> toDouble (Double_ 123.4)
-- 123.4
-- >>> toDouble (Tuple_ False 135 30 3.6)
-- 135.501
toDouble :: Angle -> Double
toDouble (Double_ f) = f
toDouble (Tuple_ False i m s) = (fromIntegral i) + (fromIntegral m) / 60.0 + s / 3600.0
toDouble (Tuple_ True i m s) = negate ( (fromIntegral i) + (fromIntegral m) / 60.0 + s / 3600.0)

-- |
-- >>> asDouble (Double_ 123.4)
-- 123.4
-- >>> asDouble (Tuple_ False 135 30 3.6)
-- 135.501
asDouble :: Angle -> Angle
asDouble f@(Double_ _) = f
asDouble x@(Tuple_ _ _ _ _) = Double_ (toDouble x)

asTuple :: Angle -> Angle
asTuple t@(Tuple_ _ _ _ _) = t
asTuple (Double_ f) | f >= 0 =
  let i = floor f
      r = 60 * (f - (fromIntegral i))
      m = floor r
      r2 = 60 * (r - (fromIntegral m))
  in
    Tuple_ False i m r2

asTuple (Double_ f) = negate $ asTuple (Double_ (- f))

instance Show Angle where
  show (Tuple_ sgn d m s)  = (if sgn then "-" else "") ++ (show d) ++ "°" ++ (show m) ++ "'" ++ (printf "%.4f" s) ++ "\""
  show (Double_ f) = show f

-- |
-- 角度の整数部分(「度]の部分)
--
-- >>> integerPart (Tuple_ True 135 2 3.5)
-- 135
-- >>> integerPart (Double_ (- 135.123))
-- 135
integerPart:: Angle -> Word8
integerPart (Tuple_ _ i _ _) = i
integerPart (Double_ f) | f >= 0 = floor f
integerPart (Double_ f)         = floor (- f)

-- |
-- >>> minutePart (Tuple_ False 135 2 3.5)
-- 2
-- >>> minutePart (Double_ 135.501)
-- 30
minutePart:: Angle -> Word8
minutePart (Tuple_ _ _ m _) = m
minutePart x@(Double_ _) = minutePart $ asTuple x

-- |
-- >>> secondPart (Tuple_ False 135 2 3.5)
-- 3.5
-- >>> Text.Printf.printf "%.6f" $ secondPart (Double_ 135.501)
-- 3.600000
secondPart:: Angle -> Double
secondPart (Tuple_ _ _ _ s) = s
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
-- >>> (read "45度12分34.56秒") == Tuple_ False 45 12 34.56
-- True
-- >>> (read "45°12'34.56\"") == Tuple_ False 45 12 34.56
-- True
--
-- 秒のマークは省略可
-- >>> (read "45:12:34.56") == Tuple_ False 45 12 34.56
-- True
--
-- 混ぜても良い。
-- >>> (read "45°12分34.56") == Tuple_ False 45 12 34.56
-- True
--
-- 数字に0を前置してよい
-- >>> (read "45°02分04.56") == Tuple_ False 45 2 4.56
-- True
--
-- 秒の小数点はあってもなくてもよい
-- >>> (read "45°02分04") == Tuple_ False 45 2 4
-- True
--
-- 秒以外には小数点を書けない
-- >>> (readMaybe "45°2.0分04秒") :: Maybe Angle
-- Nothing
-- >>> (readMaybe "45°2分4.5秒") == Just (Tuple_ False 45 2 4.5)
-- True

instance Read Angle where
  readPrec = do
    sgn <- look
    case sgn of
      ('-':_) -> do
        (L.Symbol "-") <- lexP
        d <- readPrec'
        return (negate d)
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

  return (Tuple_ (i' < 0) (abs i') m' (fromRational (L.numberToRational s)))

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
---
--- arithmatic operation on angles
---

-- |
--
-- >>> (Double_ 100.0) - (Double_ 99.0)
-- 1.0
-- >>> (Double_ 100.0) + (Double_ 99.0)
-- 199.0
--
instance Num Angle where
  a0 + a1 = Double_ $ (toDouble a0) + (toDouble a1)
  a0 - a1 = Double_ $ (toDouble a0) - (toDouble a1)
  _ * _ = undefined
  abs = Double_ . abs . toDouble
  signum = Double_ . signum . toDouble
  fromInteger n | n < 0 = Tuple_ True (- (fromInteger n)) 0 0.0
  fromInteger n = Tuple_ False (fromInteger n) 0 0.0

  negate (Double_ f) = Double_ (- f)
  negate (Tuple_ sgn i m s) = Tuple_ (not sgn) i m s

instance Eq Angle where
  (==) (Double_ a) (Double_ b) = a == b
  (==) (Tuple_ fa da ma sa) (Tuple_ fb db mb sb) = fa == fb && da == db && ma == mb && sa==sb
  (==) a b = toDouble a == toDouble b

-- |
--
-- >>> compare (Double_ 123.0) (Double_ 124.0)
-- LT
-- >>> compare (Double_ 125.0) (Double_ 124.0)
-- GT
-- >>> compare (Double_ 126.2) (Double_ 126.2)
-- EQ
-- >>> compare (Tuple_ False 135 0 1) (Tuple_ False 135 1 2)
-- LT
-- >>> compare (Tuple_ False 135 0 1) (Tuple_ False 135 0 1)
-- EQ
-- >>> compare (Tuple_ False 135 0 1) (Tuple_ True 135 0 1)
-- GT
instance Ord Angle where
  compare (Tuple_ True _ _ _) (Tuple_ False _ _ _) = LT
  compare (Tuple_ False _ _ _) (Tuple_ True _ _ _) = GT
  compare a@(Tuple_ True _ _ _) b@(Tuple_ True _ _ _) = case compare (negate a) (negate b) of
                                                          EQ -> EQ
                                                          LT -> GT
                                                          GT -> LT
  compare (Tuple_ False da ma sa) (Tuple_ False db mb sb) =
    compare da db |>= compare ma mb |>= compare sa sb
    where a |>= b = if a == EQ then b else a

  compare a b = compare (toDouble a) (toDouble b)
