{-# LANGUAGE OverloadedStrings #-}
--
--
module YamaDataSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text.Lazy as T

import YamaData
import GeoAngle

spec :: Spec
spec = do
  describe "fromList" $ do
    it "creates empty DB from null list" $ do
      YamaData.fromList [] `shouldBe` masterDbInit

    it "creates master database from a list" $ do
      let db = YamaData.fromList testList1
      length (M.keys (mdSummits db)) `shouldBe` 2
      let Just m1 = M.lookup 10000 (mdSummits db)
      primaryName m1 `shouldBe` Just "mountain1"
      pointElevation m1 `shouldBe` 500.0
      let Just m2 = M.lookup 10001 (mdSummits db)
      primaryName m2 `shouldBe` Just "mountain2"

    it "creates list definitions in mdLists" $ do
      let db = YamaData.fromList testList1
      length (M.keys (mdLists db)) `shouldBe` 1
      let list = ldPoints $ fromJust $ M.lookup 1100 (mdLists db)
      length list `shouldBe` 2
      list `shouldSatisfy` elem (1, 10000)
      list `shouldSatisfy` elem (2, 10001)

  describe "mergePointData" $ do
    it "adds new point data to master db" $ do
      let db = YamaData.fromList testList1
      let (newdb, newptid, msgs) = YamaData.mergePoint db (3, summit3)
      length (M.keys (mdSummits newdb)) `shouldBe` 3
      newptid `shouldBe` 10002

    it "updates existing summit data" $ do
      let db = YamaData.fromList testList1
      let (newdb, newptid, msgs) = YamaData.mergePoint db (4, summit4)
      length (M.keys (mdSummits newdb)) `shouldBe` 2
      newptid `shouldBe` 10000
      let Just pd = M.lookup newptid (mdSummits newdb)
      let linkref = pointLists pd
      length linkref `shouldBe` 2
      linkref `shouldBe` [(1100, 1), (1101, 4)]
      length (pointNames pd) `shouldBe` 1
      pointNames pd `shouldBe` [LocName "mountain1" ["yomi1"]]

    it "handle duplicates" $ do
      let db = YamaData.fromList testList1
      let (newdb, newptid, msgs) = YamaData.mergePoint db (5, summit1)
      length (M.keys (mdSummits newdb)) `shouldBe` 2
      newptid `shouldBe` 10000

    it "detects wrong links" $ do
      let db = YamaData.fromList testList1
      let (newdb, newptid, msgs) = YamaData.mergePoint db (5, summit5)
      length (M.keys (mdSummits newdb)) `shouldBe` 2
      newptid `shouldBe` 10000
      msgs `shouldSatisfy` elem "conflicts on link xxx for yyy"


    it "merges names on a point" $ do
      let db = YamaData.fromList testList1
      let (newdb, newptid, msgs) = YamaData.mergePoint db (6, summit6)
      let Just pd = M.lookup newptid (mdSummits newdb)
      -- names merged
      pointNames pd `shouldBe` [LocName "mountain1" ["yomi1"], LocName "mountain6" ["yomi6"]]

    it "merges yomi of names on a point" $ do
      let db = YamaData.fromList testList1
      let (newdb, newptid, msgs) = YamaData.mergePoint db (7, summit7)
      let Just pd = M.lookup newptid (mdSummits newdb)
      -- yomi merged
      length (pointNames pd) `shouldBe` 1
      pointNames pd `shouldBe` [LocName "mountain1" ["yomi1", "yomi7"]]

summit1,summit2,summit3,summit4,summit5,summit6:: PointData
summit1 = PointData (fromDouble 124) (fromDouble 35) 500.0
           [LocName "mountain1" ["yomi1"]] [] Nothing Nothing [(1100, 1)] "" ""
summit2 =PointData (fromDouble 123) (fromDouble 35.2) 400.0
           [LocName "mountain2" []] [] Nothing Nothing [(1100, 2)] "" ""
summit3 = PointData (fromDouble (124.0 + (3.0/3600.0))) (fromDouble 35) 600.0
           [LocName "mountain3" []] [] Nothing Nothing [(1100, 3)] "" ""
-- Same summit data with different name to summit1
summit4 = PointData (fromDouble (124.0 + (1.0/3600.0))) (fromDouble (35.0 + (1.0/3600.0))) 500.0
          [LocName "mountain1" []] [] Nothing Nothing [(1101,4)] "" ""

summit5 = PointData (fromDouble 124) (fromDouble 35) 500.0
           [LocName "mountain1" []] [] Nothing Nothing [(1100, 99)] "" ""

summit6 = PointData (fromDouble 124) (fromDouble 35) 500.0
           [LocName "mountain6" ["yomi6"]] [] Nothing Nothing [(1100, 1)] "" ""

summit7 = PointData (fromDouble 124) (fromDouble 35) 500.0
           [LocName "mountain1" ["yomi7"]] [] Nothing Nothing [(1100, 1)] "" ""


testList1 :: [(PointId, PointData)]
testList1 = [ (10000, summit1), (10001, summit2) ]

-- Local Variables:
-- coding: utf-8
-- End:
