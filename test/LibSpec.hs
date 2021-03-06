{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import Test.Hspec
import Lib
import Text.Regex.Posix
import qualified Data.Map as Map
import Data.Functor

test_data =  "{\"id\": \"4917495704\",\"date_taken\": \"2010-08-22 13:09:05\"}"

test_dirs = [
    "test/testdata/part1",
    "test/testdata/part2"
    ]

spec :: Spec
spec = do
    describe "json read" $ do
        it "jsonから読み込む" $
            readPhotoInfo test_data `shouldBe` (Just $ PhotoInfo { photo_id = "4917495704", date_taken = "2010-08-22 13:09:05"})
        it "jsonから読み込み失敗" $
            readPhotoInfo "" `shouldBe` Nothing
    describe "photo info map" $
        it "idに相当するデータを取得" $ do
            m <- readDirectoriesPhotoMap test_dirs
            let photo = Map.lookup "4916895077" m
            date_taken `fmap` photo `shouldBe` Just "2010-08-22 13:08:48"
    describe "photo filename to id" $ do
        it "jpgファイル"　$
            (parsePhotoFileName "../../data/data-download-30/p1080131jpg_15059676029_o.jpg") `shouldBe` Just "15059676029"
        it "movファイル"　$
             (parsePhotoFileName "../../data/data-download-30/img_0441_15007128657.mov") `shouldBe` Just "15007128657"
        