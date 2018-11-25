module Lib
    ( someFunc,
    getDirectoriesContentPaths
    ) where

import System.Directory

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- 複数ディレクトリ配下のファイルのパスを
-- ネストしないリストで返す
getDirectoriesContentPaths :: [String] -> IO [String]
getDirectoriesContentPaths paths = concat <$> mapM getDirectoryContentPaths paths

-- ディレクトリ配下のファイルのパスを返す
getDirectoryContentPaths :: String -> IO [String]
getDirectoryContentPaths path = 
    map (\x -> path ++ "/" ++ x) <$> getDirectoryContents path

