{-# LANGUAGE FlexibleInstances #-}
module Downloaders.Local (download) where

import Data.List (intercalate)
import Downloaders (DownloadFun)
import Dep (url, name)
import System.Directory (doesDirectoryExist)
import System.Process (system)
import System.Exit (ExitCode(..))

handles :: String -> IO Bool
handles = doesDirectoryExist

download :: DownloadFun
download dir dep = do
    handles' <- handles url'
    if handles'
      then do
        cp (url dep) dir
        return $ Just $ Right dep_dir
      else return Nothing
  where
    url' = url dep
    dep_dir = dir ++ (name dep)


cp :: String -> String -> IO ExitCode
cp src dest = do
    system $ cmd
  where cmd = intercalate " " ["cp", "-R", src, dest]
