module Lib
    ( someFunc,
    getDirectoriesContentPaths
    ) where

import System.Directory

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


