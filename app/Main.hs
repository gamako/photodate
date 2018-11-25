module Main where

import Lib

meta_data_dirs = [
    "../../data/72157675601142498_06440730dcc4_part1",
    "../../data/72157675601142498_06440730dcc4_part2"
    ]

main :: IO ()
main =  do
    lis <- fileListFromPaths meta_data_dirs
    print lis

