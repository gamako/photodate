module Lib
    ( someFunc,
    fileListFromPaths
    ) where

import System.Directory

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fileListFromPaths :: [String] -> IO [String]
fileListFromPaths paths = do
    ls <- mapM f paths
    return $ concat ls
        where
            f path = do
                fs <- getDirectoryContents path
                let fs2 = map (\x -> path ++ "/" ++ x) fs
                return fs2

