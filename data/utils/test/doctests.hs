module Main where

import Test.DocTest

main :: IO ()
main = doctest ["lib/GeoAngle.hs"]
