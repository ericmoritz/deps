{-# LANGUAGE FlexibleInstances #-}
module Main where
import System.IO
import Control.Monad (mapM)
import System.Directory (doesDirectoryExist)
import Data.List (isPrefixOf, break)
import qualified Dep as D

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
  return Nothing -- TODO: read

-- ===================================================================
-- Downloaders
-- ===================================================================

downloaders :: [((String -> IO Bool), D.DownloadFun)]
downloaders = [
    (is_dir,  local_downloader)
  , (is_git,  git_downloader)
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
             
        
is_dir :: String -> IO Bool
is_dir = doesDirectoryExist

local_downloader :: D.DownloadFun
local_downloader dir dep = do
    putStrLn $ "cp " ++ url ++ " " ++ dep_dir 
    return dep_dir
  where
    url = (D.url dep)
    dep_dir = dir ++ (D.name dep)

is_git :: String -> IO Bool
is_git = return . ("git://" `isPrefixOf`)

git_downloader :: D.DownloadFun
git_downloader dir dep = do
    putStrLn $ "git " ++ url ++ " " ++ dep_dir   
    return dep_dir
  where
    url = (D.url dep)
    dep_dir = dir ++ (D.name dep)
    
-- A git url looks like git://{netloc}/{path}#{dep dir}
git_dep_dir = tail . snd . break (== '#')