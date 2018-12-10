module Main where

import Lib
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Text.Parsec
import Text.Parsec.Char
import Data.Either
import System.Posix.Files
import Data.Time.Clock.POSIX

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
    photos <- getDirectoriesPhotoFilePaths photoDirBase
    -- print photoInfo
    -- let a = Map.lookup "9890096843" photoInfo
    -- print a
    forM_ photos $ \filePath -> do
        case parsePhotoFileName filePath of
            Nothing -> putStr "fail:" >> putStrLn filePath
            Just id_ -> do
                case Map.lookup id_ photoInfo of
                    Nothing ->  putStr "lookup fail:" >> putStr id_
                    Just info -> do
                        let time = dateTakenUTCTime info
                        putStr (filePath ++ " date_taken:") >> putStr (show $ date_taken info)  >> print time 
                        case time of
                            Nothing -> fail "no time"
                            Just time_ -> do
                                let posixTime = utcTimeToPOSIXSeconds time_
                                setFileTimesHiRes filePath posixTime posixTime
                            --return ()
                        --fail "hogehoge"

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
