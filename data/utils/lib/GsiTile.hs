module GsiTile (
    TileInfo(..)
  , getOffset
  , tileForCoordinate
  , url
  , extendToEast
  , extendToWest
  , extendToNorth
  , extendToSouth
  ) where

import Data.Bits (shift)
import Text.Printf

import  GeoAngle

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

yTop = lambert 85.0511

-- ランベルト関数の逆関数: グーデルマン関数
gudermann:: Floating a => a -> a
gudermann y = 180.0 / pi * asin (tanh y)


-- |
-- GSIのタイルは、経度 [-180°, 180°] 緯度 [85.0511°, -85.011°] の範囲をメルカトル図法に射影して、 縦横 2^(ズームレベル) に分割する
--
-- >>> Coordinate (Double_ 139.10177) (Double_ 35.865422)
-- Coordinate 139°6'6.372" 35°51'55.519"
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
  Right $ TileInfo xidx_ yidx_ zoom (fromDouble latNorth_) (fromDouble latSouth_) (fromDouble lonWest_) (fromDouble lonEast_) 256 256 1 1
  where
    -- split = 2 ** zoom -- ズームレベルに応じて分割される
    split = 1 `shift` zoom :: Int
    xstep = 360.0 / (fromIntegral split)
    xidx_ = floor $ (toDouble (longitude c) + 180.0) / xstep
    ystep = yTop / fromIntegral (split `div` 2)
    yidx_ = floor $ (yTop - lambert (toDouble $ latitude c)) / ystep
    latNorth_ = gudermann $ yTop - ystep * (fromIntegral yidx_)
    latSouth_ = gudermann $ yTop - ystep * (fromIntegral yidx_ + 1)
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


-- | タイル内のピクセル位置に対応する経度緯度
--
getCoord :: TileInfo -> (Int, Int) -> Coordinate
getCoord tile (x, y) = Coordinate lon lat
  where
    tiEast = (toDouble $ lonEast tile)
    tiWest = (toDouble $ lonWest tile)
    tiNorth = (toDouble $ latNorth tile)
    tiSouth = (toDouble $ latSouth tile)
    width = tiEast - tiWest
    lon = fromDouble (tiWest + width * (fromIntegral x) / (fromIntegral (xpixels tile)))
    -- XXX use lambert and gudermann
    height = tiNorth - tiSouth
    lat = fromDouble (tiNorth - height * (fromIntegral y) / (fromIntegral (ypixels tile)))

-- |
-- タイルを複数つないで大きくする
--
-- >>> let (Right t) = tileForCoordinate 17 $ Coordinate (fromDouble 139.10177) (fromDouble 35.865422)
-- >>> let (Right te) = tileForCoordinate 17 $ Coordinate (fromDouble 139.103) (fromDouble 35.865422)
-- >>> let (Right tw) = tileForCoordinate 17 $ Coordinate (fromDouble 139.099) (fromDouble 35.865422)
-- >>> let (Right tn) = tileForCoordinate 17 $ Coordinate (fromDouble 139.103) (fromDouble 35.867)
-- >>> let (Right ts) = tileForCoordinate 17 $ Coordinate (fromDouble 139.103) (fromDouble 35.863)
-- >>> let t' = extendToEast t
-- >>> (xspan t', yspan t', xpixels t', ypixels t', zoomLevel t')
-- (2,1,512,256,17)
-- >>> (xidx t', yidx t', lonWest t', latNorth t', latSouth t') == (xidx t, yidx t, lonWest t, latNorth t, latSouth t)
-- True
-- >>> lonWest t' == lonWest te
-- True
-- >>> let t' = extendToWest t
-- >>> (xspan t', yspan t', xpixels t', ypixels t', zoomLevel t')
-- (2,1,512,256,17)
-- >>> (xidx t', yidx t', lonEast t', latNorth t', latSouth t') == (xidx t - 1, yidx t, lonEast t, latNorth t, latSouth t)
-- True
-- >>> lonWest t' == lonWest tw
-- True
-- >>> let t' = extendToSouth t
-- >>> (xspan t', yspan t', xpixels t', ypixels t', zoomLevel t')
-- (1,2,256,512,17)
-- >>> (xidx t', yidx t', lonWest t', lonEast t', latNorth t') == (xidx t, yidx t, lonWest t, lonEast t, latNorth t)
-- True
-- >>> ((printf "%.10f" (toDouble $ latSouth t')) :: String) == printf "%.10f" (toDouble $ latSouth ts)
-- True

extendToEast:: TileInfo -> TileInfo
extendToEast tile@(TileInfo xidx_ yidx_ zoom north south west _ xpix ypix xspan_ yspan_) =
  TileInfo xidx_ yidx_ zoom north south west newEast (xpix + 256) ypix (xspan_ + 1) yspan_
  where
    Coordinate newEast _ = getCoord tile ((xpix + 256), 0)


extendToWest:: TileInfo -> TileInfo
extendToWest tile@(TileInfo xidx_ yidx_ zoom north south _ east xpix ypix xspan_ yspan_) =
  TileInfo (xidx_ -1) yidx_ zoom north south newWest east (xpix + 256) ypix (xspan_ + 1) yspan_
  where
    Coordinate newWest _ = getCoord tile (- 256, 0)


extendToNorth:: TileInfo -> TileInfo
extendToNorth (TileInfo xidx_ yidx_ zoom north south west east xpix ypix xspan_ yspan_) =
  TileInfo xidx_ (yidx_ - 1) zoom newNorth south west east xpix (ypix + 256) xspan_ (yspan_ + 1)

  where
    split = 1 `shift` zoom :: Int
    ystep = yTop / fromIntegral (split `div` 2)
    newNorth = fromDouble $ gudermann $ lambert (toDouble north) + ystep

extendToSouth:: TileInfo -> TileInfo
extendToSouth (TileInfo xidx_ yidx_ zoom north south west east xpix ypix xspan_ yspan_) =
  TileInfo xidx_ yidx_ zoom north newSouth west east xpix (ypix + 256) xspan_ (yspan_ + 1)

  where
    split = 1 `shift` zoom :: Int
    ystep = yTop / fromIntegral (split `div` 2)
    newSouth = fromDouble $ gudermann $ lambert (toDouble south) - ystep
