{-# LANGUAGE FlexibleInstances #-}
module Downloaders.Git (is, download) where

import Dep (DownloadFun, url, name)
import Data.List (isPrefixOf, break)

is :: String -> IO Bool
is = return . ("git://" `isPrefixOf`)

download :: DownloadFun
download dir dep = do
    putStrLn $ "git " ++ url dep ++ " " ++ dep_dir   -- TODO: Actually clone the repo
    return dep_dir
  where
    dep_dir = dir ++ (name dep)
