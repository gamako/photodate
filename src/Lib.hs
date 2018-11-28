{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc,
    getDirectoriesContentPaths,
    PhotoInfo (..),
    readPhotoInfo
    ) where

import Data.Aeson
import Control.Applicative
import System.Directory
import qualified Data.ByteString.Lazy.Internal as S

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- 複数ディレクトリ配下のファイルのパスを
-- ネストしないリストで返す
-- 第一引数はフィルタリング関数
getDirectoriesContentPaths :: (String -> Bool) -> [String] -> IO [String]
getDirectoriesContentPaths f paths= concat <$> mapM (getDirectoryContentPaths f) paths

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
readPhotoInfo :: S.ByteString -> Maybe PhotoInfo
readPhotoInfo = decode
