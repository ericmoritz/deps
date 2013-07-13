{-# LANGUAGE FlexibleInstances #-}
module Main where
import System.IO
import Control.Monad (mapM, liftM)
import qualified Scanner
import qualified Dep as D
import qualified Downloaders.Local as Local
import qualified Downloaders.Git as Git

main = 
  process "./deps/" ["./deps.txt"]

process _ [] = return ()
process dir files = 
    Scanner.scan files >>= -- [D.Dep]
    download_urls      >>= -- [String]
    dep_files          >>=
    process dir            -- recurse
  where
    download_urls = mapM (download dir)
    dep_files     = liftM only_just . mapM dep_file
    only_just ms  = [v | Just v <- ms]

dep_file :: String -> IO (Maybe String)
dep_file dep_dir = 
  return Nothing -- TODO: look in dep_dir for a deps.txt

-- ===================================================================
-- Downloaders
-- ===================================================================
-- TODO: move this stuff to Downloaders.Download

-- Downloads the dependancy and returns its directory
download :: String -> D.Dep -> IO String
download dir dep =
  downloader (D.url dep) >>= \f -> f dir dep

downloader :: String -> IO D.DownloadFun
downloader = choose_downloader downloaders
  where
    choose_downloader [] url = do
      error  $ "Unrecognizeable source URL: " ++ url
    choose_downloader (x:xs) url = do
      let (test,fun) = x
      b <- (test url)
      if b 
        then return fun
        else choose_downloader xs url
             
downloaders :: [((String -> IO Bool), D.DownloadFun)]
downloaders = [
    (Local.is,  Local.download)
  , (Git.is,  Git.download)
  ]

