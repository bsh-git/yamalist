module Main where

import qualified GsiTile (someFunc)
import qualified GeoAngle (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  GsiTile.someFunc
  GeoAngle.someFunc
