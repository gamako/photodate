{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lib
    ( getDirectoriesContentPaths
    , photoInfoFileFilter
    , getDirectoriesPhotoInfoPaths
    , getDirectoriesPhotoFilePaths
    , PhotoInfo (..)
    , readPhotoInfo
    , readDirectoriesPhotoInfo
    , readDirectoriesPhotoMap
    ) where

import Data.Aeson
import Text.Regex.Posix
import Control.Applicative
import System.Directory
import qualified Data.ByteString.Lazy.Internal as LSI
import qualified Data.ByteString.Lazy as LS
import qualified Data.Map as Map
import Data.Either
import Debug.Trace


photoInfoFileFilter :: String -> Bool
photoInfoFileFilter x = (x =~ r :: [[String]]) /= []
    where
        r = "photo_.*\\.json" :: String

-- 複数ディレクトリ配下のファイルのパスを
-- ネストしないリストで返す
-- 第一引数はフィルタリング関数
getDirectoriesContentPaths :: (String -> Bool) -> [FilePath] -> IO [FilePath]
getDirectoriesContentPaths f paths = concat <$> mapM (getDirectoryContentPaths f) paths

-- ディレクトリ配下のファイルのパスを返す
-- 第一引数はフィルタリング関数
getDirectoryContentPaths :: (String -> Bool) -> String -> IO [FilePath]
getDirectoryContentPaths f path = 
    map (\x -> path ++ "/" ++ x) . filter f <$> getDirectoryContents path

-- 複数ディレクトリ配下の写真メタデータファイルのパスを
-- ネストしないリストで返す
getDirectoriesPhotoInfoPaths :: [FilePath] -> IO [FilePath]
getDirectoriesPhotoInfoPaths paths = concat <$> mapM (getDirectoryContentPaths photoInfoFileFilter) paths

-- 写真ファイルの一覧を作成する
getDirectoriesPhotoFilePaths :: FilePath -> IO [FilePath]
getDirectoriesPhotoFilePaths path = do
    dirs <- map (\x -> path ++ "/" ++ x) . filter dirFilter <$> getDirectoryContents path
    concat <$> mapM getFiles dirs
        where
            dirFilter :: String -> Bool
            dirFilter x = (x =~ ("^data-download-[0-9]+$" :: String) :: [[String]]) /= []
            getFiles x = map (\y -> x ++ "/" ++ y) <$> getDirectoryContents x

-- jsonから読み込むファイル情報
data PhotoInfo = PhotoInfo { photo_id :: String, date_taken :: String } deriving (Show, Eq)

-- aesonを使ってパースする定義
instance FromJSON PhotoInfo where
    parseJSON (Object v) = PhotoInfo <$> (v .: "id")
                                     <*> (v .: "date_taken")

-- jsonから変換
readPhotoInfo :: LS.ByteString -> Maybe PhotoInfo
readPhotoInfo = decode


readDirectoriesPhotoInfo :: [FilePath] -> IO [Either String PhotoInfo]
readDirectoriesPhotoInfo ds = do
        files <- getDirectoriesPhotoInfoPaths ds
        mapM readOrError files
    where
        readOrError :: FilePath -> IO (Either String PhotoInfo)
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

readDirectoriesPhotoMap ::  [FilePath] -> IO (Map.Map String PhotoInfo)
readDirectoriesPhotoMap dirs = do
    ps <- readDirectoriesPhotoInfo dirs
    let ps' = rights ps
    let m = makePhotoMap ps'
    return m
