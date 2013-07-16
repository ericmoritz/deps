{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Monad (liftM, filterM, forM_, unless)
import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Downloaders (DownloadFun)
import Debug.Trace (traceShow)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)
import qualified Scanner
import qualified Dep as D
import qualified Downloaders.Local as Local
import qualified Downloaders.Git as Git

traceShow' a = traceShow a a

main :: IO ()
main = 
  process "./deps/" ["./deps.txt"]

process :: FilePath -> [FilePath] -> IO ()
process _ [] = return ()
process dir files = 
    mkDir dir          >>
    Scanner.scan files >>= -- [D.Dep]
    download_urls      >>= -- [Either String String]
    log_errors         >>= -- [String]
    dep_files          >>=
    process dir            -- recurse
  where
    mkDir :: String -> IO ()
    mkDir dir = doesDirectoryExist dir >>=
                flip unless (createDirectory dir)
    download_urls :: [D.Dep] -> IO [Either String String]
    download_urls = mapM (download dir)

    dep_files :: [String] -> IO [String]
    dep_files fs = liftM catMaybes $ mapM dep_file fs

    log_errors :: [(Either String String)] -> IO [String]
    log_errors items = do
      let errors = (lefts items)
      if null errors
        then return $ rights items
        else error $ intercalate "\n" errors

                           
dep_file :: String -> IO (Maybe String)
dep_file dep_dir = do
    exists' <- doesFileExist dep_file'

    if exists'
      then return $ Just dep_file'
      else return Nothing
  where dep_file' = dep_dir </> "deps.txt"

-- TODO: Find where the </> function actually exists
(</>) :: String -> String -> String
x </> y = x ++ "/" ++ y

-- ===================================================================
-- Downloaders
-- ===================================================================
-- TODO: move this stuff to Downloaders.Download

-- Downloads the dependancy and returns its directory

download :: String -> D.Dep -> IO (Either String String)
download dir dep = do
    dep_dir' <- firstJustM downloads

    return $ case dep_dir' of
          Just v       -> v
          Nothing      -> Left errorline

  where
    dep_dir   = dir </> (D.name dep)
    downloads = map (\f -> f dir dep) downloaders
    url'      = D.url dep

    errorline :: String
    errorline = D.errorstr ("Unrecognizeable source URL " ++ url') dep

downloaders :: [DownloadFun]
downloaders = [exists, Local.download, Git.download]

-- Pass through downloader for existing directories
exists :: DownloadFun
exists dir dep = do
  exists <- doesDirectoryExist dep_dir
  if exists
    then return $ Just $ Right dep_dir
    else return Nothing
  where dep_dir = dir ++ (D.name dep)
        
firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (mx:xs) = mx >>= \x ->
    case x of
      Just _ -> return x
      Nothing -> firstJustM xs
