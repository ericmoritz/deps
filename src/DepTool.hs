{-# LANGUAGE FlexibleInstances #-}
module Main where
import System.IO
import Control.Monad (mapM)

import qualified Dep as D
import qualified Downloaders.Local as Local
import qualified Downloaders.Git as Git

main = 
  process "./deps/" ["./deps.txt"]

  
process _ [] = return ()
process dir (x:xs) = 
    urls x        >>= -- [D.Dep]
    download_urls >>= -- [String]
    dep_files     >>= -- [Just String]
    only_just     >>= -- [String]
    enqueue       >>= -- xs ++ [String]
    process dir       -- recurse
  where
    enqueue       = return . (xs ++)
    download_urls = mapM (D.download dir)
    dep_files     = mapM dep_file
    only_just ms  = return [v | Just v <- ms]

urls f = do
  content <- readFile f
  mapM line2dep $ lines content

line2dep :: String -> IO D.Dep
line2dep line = do 
    d <- downloader url
    return $ D.Dep name url d
  where
    (name, url) = parse_dep line

parse_dep line = 
  (name, url)
  where
    bits = words line
    name = head bits
    url  = last bits

dep_file :: String -> IO (Maybe String)
dep_file dep_dir = 
  return Nothing -- TODO: look in dep_dir for a deps.txt

-- ===================================================================
-- Downloaders
-- ===================================================================

downloaders :: [((String -> IO Bool), D.DownloadFun)]
downloaders = [
    (Local.is,  Local.download)
  , (Git.is,  Git.download)
  ]

downloader :: String -> IO D.DownloadFun
downloader = choose_downloader downloaders
  where
    choose_downloader [] line = do
      error  $ "Unrecognizeable source URL: " ++ line
    choose_downloader (x:xs) line = do
      let (test,fun) = x
      b <- (test line)
      if b 
        then return fun
        else choose_downloader xs line
             
