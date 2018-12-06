module Main where

import Lib
import Control.Monad
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Char

metaDataDirs = [
    "../../data/72157675601142498_06440730dcc4_part1",
    "../../data/72157675601142498_06440730dcc4_part2"
    ]

testDirs = [
    "test/testdata/part1",
    "test/testdata/part2"
    ]

photoDirBase = "../../data"

main :: IO ()
main =  do
    photoInfo <- readDirectoriesPhotoMap metaDataDirs
    let a = Map.lookup "10038326853" photoInfo
    photos <- getDirectoriesPhotoFilePaths photoDirBase
    forM_ photos $ \x -> do
        let photoId = parsePhotoFileName x
        case photoId of
            Just photoId -> return ()
            Nothing -> print $ x ++ ":xxxxxxxxxxxxxxxxxxxxxxx"
        
