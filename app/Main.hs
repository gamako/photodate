module Main where

import Lib
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Text.Parsec
import Text.Parsec.Char
import Data.Either

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
    -- print photoInfo
    -- let a = Map.lookup "9890096843" photoInfo
    -- print a
    photos <- getDirectoriesPhotoFilePaths photoDirBase
    let ids = mapMaybe parsePhotoFileName photos
    print ids


-- チェック用
-- main :: IO ()
-- main = check

check :: IO ()
check = do
    r <- readDirectoriesPhotoInfo metaDataDirs
    let parseFails = lefts r
    -- パース失敗したものを表示
    putStr "parse fails:"
    print parseFails
    let photoInfo = rights r
    -- ファイルの一覧を生成
    photos <- getDirectoriesPhotoFilePaths photoDirBase
    forM_ photos $ \x -> do
        let r = parsePhotoFileName x
        case r of
            Nothing -> do
                putStr "parse error:"
                putStrLn x
            Just id_ -> return ()
