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
    , parsePhotoFileName
    ) where

import Data.Aeson
import Text.Regex.Posix
import Control.Applicative
import System.Directory
import qualified Data.ByteString.Lazy.Internal as LSI
import qualified Data.ByteString.Lazy as LS
import qualified Data.Map as Map
import Data.Either
import Data.Either.Combinators
import Data.Char (isDigit)
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Text.Parsec
import Text.Parsec.Char
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
            getFiles = getDirectoryContentPaths f
                where
                    f x = not $ elem x [".", ".."]

-- jsonから読み込むファイル情報
data PhotoInfo = PhotoInfo { photo_id :: String, filename :: String, date_taken :: String } deriving (Show, Eq)

dateTakenUTCTime :: PhotoInfo -> Maybe UTCTime
dateTakenUTCTime x = parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M:%S" $ date_taken x

-- aesonを使ってパースする定義
instance FromJSON PhotoInfo where
    parseJSON (Object v) = do
        id_ <- (v .: "id")
        date_taken <- (v .: "date_taken")
        original <-  (v .: "original")
        case filenameFromUrl original of
            Left x -> fail $ show x
            Right filename -> do
                return $ PhotoInfo id_ filename date_taken

-- urlのファイル名部分を抜き出す
filenameFromUrl :: String -> Either ParseError String
filenameFromUrl x = parse p "hoge" x
    where
        p = do
            t <- sepBy1 (Text.Parsec.many (noneOf "/")) (char '/')
            return $ last t

-- jsonから変換
readPhotoInfo :: LS.ByteString -> Maybe PhotoInfo
readPhotoInfo = decode

-- ファイル群からphotoInfoのリストを作成する
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

-- ディレクトリからidをキーとしたphotoInfoのマップを作成する
readDirectoriesPhotoMap ::  [FilePath] -> IO (Map.Map String PhotoInfo)
readDirectoriesPhotoMap dirs = do
    ps <- readDirectoriesPhotoInfo dirs
    let ps' = rights ps
    return $ makePhotoMap ps'


-- ファイルに含まれたIDをパースする
parsePhotoFileName :: String -> Maybe String
parsePhotoFileName x = rightToMaybe $ parse p "hoge" x
    where
        p = do
            dirs
            c <- filename
            let d = parse fileParse "hoge2" c
            case d of
                Left _ -> fail "no ID included"
                Right x -> case idFromTokens x of
                    Nothing -> fail "no ID included"
                    Just x -> return x

                where
                    dir = many1 $ noneOf "/"
                    dirs = skipMany $ try $ dir >> char '/'
                    filename = manyTill anyChar (try (char '.' >> many1 (noneOf ".") >> eof))
                    fileParse = sepBy (many1 (noneOf "_")) $ char '_'
                    -- ファイル名を'_'区切りにしたものからidを抜き出す
                    idFromTokens :: [String] -> Maybe String
                    idFromTokens xs = idFromTokens_ $ reverse xs
                        where
                            idFromTokens_ :: [String] -> Maybe String
                            idFromTokens_ [] = Nothing
                            idFromTokens_ ("o":xs) = idFromTokens_ xs
                            idFromTokens_ (x:xs) = case (isValidId x) of
                                True -> Just x
                                False -> idFromTokens_ xs
                            isValidId x = all isDigit x && length x > 9
                    

 