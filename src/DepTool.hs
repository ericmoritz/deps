{-# LANGUAGE FlexibleInstances #-}
module Main where
import System.IO
import qualified Config as C
import qualified Dep as D
import Control.Monad (mapM)
import System.Directory (doesDirectoryExist)
import Data.List (isPrefixOf, break)
import System.FilePath.Posix (dropExtension, takeFileName)

type DownloadFun = D.Dep -> String -> IO String

main = do
  config <- make_config
  download_deps (C.dir config) (C.dep_files config)


make_config :: IO C.Config
make_config =
  return $ C.config "./deps/" ["./deps.txt"]


download_deps :: String -> [String] -> IO ()
download_deps _ [] = do
  return ()
download_deps dir (current:rest) = do
  deps <- parse_dep_file current
  dirs <- download_all dir deps
  more_files <- collect_dep_files dirs
  download_deps dir (rest ++ more_files)

  
parse_dep_file :: String -> IO [D.Dep]
parse_dep_file f = do
  contents <- readFile f
  mapM parse_dep $ lines contents


parse_dep :: String -> IO D.Dep
parse_dep line = do
  d <- downloader line
  return $ D.new line d

downloaders :: [((String -> IO Bool), DownloadFun)]
downloaders = [
    (is_dir,  local_downloader)
  , (is_git,  git_downloader)
  ]

downloader :: String -> IO DownloadFun
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
             
        
  
download_all :: String -> [D.Dep] -> IO [String]
download_all dir [] = do
  return []
download_all dir (current:rest) =
  do
    dep_dir <- D.download current dir
    more <- download_all dir rest
    return $ (dep_dir:more)


collect_dep_files :: [String] -> IO [String]
collect_dep_files dirs = do
  return []

is_dir :: String -> IO Bool
is_dir = doesDirectoryExist

local_downloader :: DownloadFun
local_downloader dep dir = do
    putStrLn $ "cp " ++ url ++ " " ++ dep_dir 
    return dep_dir
  where
    url = (D.url dep)
    dep_dir = dir ++ url

is_git :: String -> IO Bool
is_git = return . ("git://" `isPrefixOf`)

git_downloader :: D.Dep -> String -> IO String
git_downloader dep dir = do
    putStrLn $ "git " ++ url ++ " " ++ dep_dir   
    return dep_dir
  where
    url = (D.url dep)
    dep_dir = git_dep_dir url
    
-- A git url looks like git://{netloc}/{path}#{dep dir}
git_dep_dir = tail . snd . break (== '#')