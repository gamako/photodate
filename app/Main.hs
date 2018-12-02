module Main where

import Lib
import qualified Data.Map as Map

metaDataDirs = [
    "../../data/72157675601142498_06440730dcc4_part1",
    "../../data/72157675601142498_06440730dcc4_part2"
    ]

testDirs = [
    "test/testdata/part1",
    "test/testdata/part2"
    ]

main :: IO ()
main =  do
    lis <- readDirectoriesPhotoMap metaDataDirs
    let a = Map.lookup "10038326853" lis
    print a

