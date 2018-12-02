module Main where

import Lib
import qualified Data.Map as Map

meta_data_dirs = [
    "../../data/72157675601142498_06440730dcc4_part1",
    "../../data/72157675601142498_06440730dcc4_part2"
    ]

test_dirs = [
    "../../testdata/part1",
    "../../testdata/part2"
    ]

main :: IO ()
main =  do
    lis <- readDirectoriesPhotoMap meta_data_dirs
    let a = Map.lookup "10038326853" lis
    print a

