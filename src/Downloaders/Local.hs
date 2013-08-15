{-# LANGUAGE FlexibleInstances #-}
module Downloaders.Local (download) where

import Data.List (intercalate)
import Downloaders (Download)
import Dep (url, name, Dep)
import System.Directory (doesDirectoryExist)
import System.Process (system)
import System.Exit (ExitCode(..))
import Control.Monad.Trans
import Control.Monad

handles :: String -> IO Bool
handles = doesDirectoryExist

download :: String -> Dep -> Download String
download dir dep = do
    guard =<< (liftIO $ handles url')
    liftIO $ cp (url dep) dep_dir
    return dep_dir
  where
    url' = url dep
    dep_dir = dir ++ (name dep)


cp :: String -> String -> IO ExitCode
cp src dest = do
  system $ intercalate " " ["cp", "-R", src, dest]
  return ExitSuccess
