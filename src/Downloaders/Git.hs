{-# LANGUAGE FlexibleInstances #-}
module Downloaders.Git (download) where

import Downloaders (DownloadFun)
import Dep (url, name)
import Data.List (isPrefixOf)

handles :: String -> Bool
handles = ("git://" `isPrefixOf`)

download :: DownloadFun
download dir dep =
  if handles url'
  then do
    putStrLn $ "git " ++ url dep ++ " " ++ dep_dir   -- TODO: Actually clone the repo
    return $ Just dep_dir
  else return Nothing
  where
    url' = url dep
    dep_dir = dir ++ (name dep)
