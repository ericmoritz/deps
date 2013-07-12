{-# LANGUAGE FlexibleInstances #-}
module Downloaders.Local (is, download) where

import Dep (DownloadFun, url, name)
import System.Directory (doesDirectoryExist)

is :: String -> IO Bool
is = doesDirectoryExist

download :: DownloadFun
download dir dep = do
    putStrLn $ "cp " ++ url dep ++ " " ++ dep_dir  -- TODO: Actually copy the tree
    return dep_dir
  where
    dep_dir = dir ++ (name dep)
