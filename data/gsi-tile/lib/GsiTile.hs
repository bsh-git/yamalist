module GsiTile (
    TileInfo(..)
  , getOffset
  , tileForCoordinate
  , url
  ) where

import Data.Bits (shift)
import Text.Printf

import GeoAngle

data TileInfo = TileInfo
  { xidx :: Int
  , yidx :: Int
  , zoomLevel :: Int
  , latNorth :: Angle
  , latSouth :: Angle
  , lonWest :: Angle
  , lonEast :: Angle
  , xpixels :: Int -- width of the tile in pixel (256)
  , ypixels :: Int -- height of the tile in pixel (256)
  , xspan :: Int   -- horizontal join
  , yspan :: Int   -- vertical join
  } deriving Show


-- メルカルト図法のY軸: ランベルト関数
lambert:: Floating a => a -> a
lambert deg = log $ tan $ pi/4 + pi*deg/180.0/2

-- ランベルト関数の逆関数: グーデルマン関数
gudermann:: Floating a => a -> a
gudermann y = 180.0 / pi * asin (tanh y)


-- |
-- GSIのタイルは、経度 [-180°, 180°] 緯度 [85.0511°, -85.011°] の範囲をメルカトル図法に射影して、 縦横 2^(ズームレベル) に分割する
--  
-- >>> Coordinate (Double_ 139.10177) (Double_ 35.865422)
-- Coordinate (Double_ 139.10177) (Double_ 35.865422)
--
-- >>> let (Right t) = tileForCoordinate 17 $ Coordinate (Double_ 139.10177) (Double_ 35.865422)
-- >>> (xidx t, yidx t)
-- (116181,51530)
-- >>> (xpixels t, ypixels t, xspan t, yspan t)
-- (256,256,1,1)
--
tileForCoordinate :: Int -> Coordinate -> Either String TileInfo
tileForCoordinate zoom _ | zoom < 0 = Left "Bad zoom"
tileForCoordinate zoom _ | zoom > 18 = Left "Too big zoom"
tileForCoordinate zoom c =
  Right $ TileInfo xidx_ yidx_ zoom (Double_ latNorth_) (Double_ latSouth_) (Double_ lonWest_) (Double_ lonEast_) 256 256 1 1
  where
    -- split = 2 ** zoom -- ズームレベルに応じて分割される
    split = 1 `shift` zoom :: Int
    xstep = 360.0 / (fromIntegral split)
    xidx_ = floor $ (toDouble (longitude c) + 180.0) / xstep
    y_top = lambert 85.0511
    ystep = y_top / fromIntegral (split `div` 2)
    yidx_ = floor $ (y_top - lambert (toDouble $ latitude c)) / ystep
    latNorth_ = gudermann $ y_top - ystep * (fromIntegral yidx_)
    latSouth_ = gudermann $ y_top - ystep * (fromIntegral yidx_ + 1)
    lonWest_ = xstep * (fromIntegral xidx_) - 180.0
    lonEast_ = lonWest_ + xstep

-- |
-- タイルを取得するためのURL
--
-- >>> let (Right t) = tileForCoordinate 17 $ Coordinate (Double_ 139.10177) (Double_ 35.865422)
-- >>> url t 0 0
-- "https://cyberjapandata.gsi.go.jp/xyz/std/17/116181/51530.png"
--
url :: TileInfo -> Int -> Int -> String
url ti xspanidx yspanidx =
    printf "https://cyberjapandata.gsi.go.jp/xyz/std/%d/%d/%d.png" zoom x y
  where
    zoom = zoomLevel ti
    x = xspanidx + xidx ti
    y = yspanidx + yidx ti


-- | 経度緯度の地点のタイルに対する相対位置 (in pixels)
--
-- XXX
-- >>> let (Right t) = tileForCoordinate 17 $ Coordinate (Double_ 139.10177) (Double_ 35.865422)
-- >>> getOffset t (Coordinate (Double_ 139.10177) (Double_ 35.865422))
-- (104,151)
--
getOffset :: TileInfo -> Coordinate -> (Int, Int)
getOffset tile c = (xoff, yoff)
  where
    tiEast = (toDouble $ lonEast tile)
    tiWest = (toDouble $ lonWest tile)
    tiNorth = (toDouble $ latNorth tile)
    tiSouth = (toDouble $ latSouth tile)
    lon = toDouble $ longitude c
    lat = toDouble $ latitude c
    width = tiEast - tiWest
    xoff = floor $ (fromIntegral (xpixels tile)) * (lon - tiWest) / width
    height = tiNorth - tiSouth
    yoff = floor $ (fromIntegral (ypixels tile)) * (tiNorth - lat) / height
