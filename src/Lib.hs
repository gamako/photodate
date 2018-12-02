{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lib
    ( someFunc,
    getDirectoriesContentPaths,
    PhotoInfo (..),
    readPhotoInfo,
    readDirectoriesPhotoInfo
    ) where

import Data.Aeson
import Control.Applicative
import System.Directory
import qualified Data.ByteString.Lazy.Internal as LSI
import qualified Data.ByteString.Lazy as LS
import qualified Data.Map as Map
import Data.Either
import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- 複数ディレクトリ配下のファイルのパスを
-- ネストしないリストで返す
-- 第一引数はフィルタリング関数
getDirectoriesContentPaths :: (String -> Bool) -> [String] -> IO [String]
getDirectoriesContentPaths f paths = concat <$> mapM (getDirectoryContentPaths f) paths

-- ディレクトリ配下のファイルのパスを返す
-- 第一引数はフィルタリング関数
getDirectoryContentPaths :: (String -> Bool) -> String -> IO [String]
getDirectoryContentPaths f path = 
    map (\x -> path ++ "/" ++ x) . filter f <$> getDirectoryContents path


-- jsonから読み込むファイル情報
data PhotoInfo = PhotoInfo { photo_id :: String, date_taken :: String } deriving (Show, Eq)

-- aesonを使ってパースする定義
instance FromJSON PhotoInfo where
    parseJSON (Object v) = PhotoInfo <$> (v .: "id")
                                     <*> (v .: "date_taken")

-- jsonから変換
readPhotoInfo :: LS.ByteString -> Maybe PhotoInfo
readPhotoInfo = decode


readDirectoriesPhotoInfo :: (String -> Bool) -> [String] -> IO [Either String PhotoInfo]
readDirectoriesPhotoInfo f ds = do
        files <- getDirectoriesContentPaths f ds
        mapM readOrError files
    where
        readOrError :: String -> IO (Either String PhotoInfo)
        readOrError path = do
            content <- LS.readFile path
            let !info = readPhotoInfo content
            return $ case info of
                 Just x -> Right x
                 _ -> Left path

makePhotoMap :: [PhotoInfo] -> Map.Map String PhotoInfo
makePhotoMap xs = Map.fromListWith (\x y -> x) xs'
    where
        xs' = map (\x -> (photo_id x, x) ) xs

readDirectoriesPhotoMap ::  (String -> Bool) -> [String] -> IO (Map.Map String PhotoInfo)
readDirectoriesPhotoMap f dirs = do
    ps <- readDirectoriesPhotoInfo f dirs
    let ps' = rights ps
    let m = makePhotoMap ps'
    return m