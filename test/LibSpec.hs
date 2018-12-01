{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import Test.Hspec
import Lib
import Text.Regex.Posix

test_data =  "{\"id\": \"4917495704\",\"date_taken\": \"2010-08-22 13:09:05\"}"

test_dirs = [
    "../../testdata/part1",
    "../../testdata/part2"
    ]

spec :: Spec
spec = do
    describe "json read" $ do
        it "jsonから読み込む" $ do
            readPhotoInfo test_data `shouldBe` (Just $ PhotoInfo { photo_id = "4917495704", date_taken = "2010-08-22 13:09:05"})
        it "jsonから読み込み失敗" $ do
            readPhotoInfo "" `shouldBe` Nothing


