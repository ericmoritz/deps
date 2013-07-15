{-# LANGUAGE FlexibleInstances #-}
module Downloaders.Local (download) where

import Downloaders (DownloadFun)
import Dep (url, name)
import System.Directory (doesDirectoryExist)

handles :: String -> IO Bool
handles = doesDirectoryExist

download :: DownloadFun
download dir dep = do
    handles' <- handles url'
    if handles'
      then do
        putStrLn $ "cp " ++ url dep ++ " " ++ dep_dir  -- TODO: Actually copy the tree
        return $ Just dep_dir
      else return Nothing
  where
    url' = url dep
    dep_dir = dir ++ (name dep)
