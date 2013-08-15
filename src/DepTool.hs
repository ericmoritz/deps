{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Monad (liftM, filterM, forM_, msum, guard, unless)
import Control.Monad.Trans
import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)
import Downloaders (Download, runDownload)
import Data.List (intercalate)
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


download :: String -> D.Dep -> IO (Either String String)
download dir dep = do
  dep_dir <- runDownload $ downloader dir dep
  return $ maybe (Left errorline) Right dep_dir
  where
    dep_dir   = dir </> (D.name dep)
    url'      = D.url dep
    errorline = D.errorstr ("Unrecognizeable source URL " ++ url') dep
             
downloader :: String -> D.Dep -> Download String
-- chooses the first appropriate downloader
downloader dir dep = msum $ [
  exists dir dep,
  Local.download dir dep,
  Git.download dir dep]

-- Dummy downloader for existing directories
exists :: String -> D.Dep -> Download String
exists dir dep = do
  guard =<< (liftIO . doesDirectoryExist) dep_dir
  return dep_dir
  where
    dep_dir = dir ++ (D.name dep)
