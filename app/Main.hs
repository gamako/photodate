module Main where

import Lib
import Text.Regex.Posix

meta_data_dirs = [
    "../../data/72157675601142498_06440730dcc4_part1",
    "../../data/72157675601142498_06440730dcc4_part2"
    ]

file_filter :: String -> Bool
file_filter x = (x =~ "photo_.*\\.json" :: [[String]]) /= []

main :: IO ()
main =  do
    lis <- getDirectoriesContentPaths file_filter meta_data_dirs
    print lis

