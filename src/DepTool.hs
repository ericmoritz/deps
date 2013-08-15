{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Monad (liftM, filterM, forM_, msum, guard)
import Control.Monad.Trans
import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)
import Downloaders (Download, runDownload)
import Debug.Trace (traceShow)
import System.Directory (doesFileExist, doesDirectoryExist)
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
    Scanner.scan files >>= -- [D.Dep]
    download_urls      >>= -- [Either String (IO String)]
    log_errors         >>= -- [String]
    dep_files          >>=
    process dir            -- recurse
  where
    download_urls :: [D.Dep] -> IO [Either String String]
    download_urls = mapM (download dir)

    dep_files :: [String] -> IO [String]
    dep_files fs    = liftM catMaybes $ mapM dep_file fs

    log_errors :: [Either String String] -> IO [String]
    log_errors items = do
      forM_ (lefts items) putStrLn
      return $ rights items
                           
dep_file :: String -> IO (Maybe String)
dep_file dep_dir = do
    exists <- doesFileExist dep_file
    if exists
      then return $ Just dep_file
      else return Nothing
  where dep_file = dep_dir </> "deps.txt"

-- TODO: Find where this function actually exists
(</>) x y = x ++ "/" ++ y

-- ===================================================================
-- Downloaders
-- ===================================================================
-- TODO: move this stuff to Downloaders.Download

-- Downloads the dependancy and returns its directory

download :: String -> D.Dep -> IO (Either String String)
download dir dep = do
  dep_dir <- runDownload $ downloader dir dep
  return $ maybe (Left $ "Unrecognizeable source URL " ++ fileline ++ url') Right dep_dir
  where
    dep_dir   = dir </> (D.name dep)
    url'      = D.url dep
    fileline  = (D.fileName dep) ++ ":" ++ show (D.line dep) ++ ":"

--downloader :: String -> IO (Either String DownloadFun)
--downloader url =
--
--  where
--    choose_downloader ds _ =
--      mapM (test url) ds
--    test u (t,_) = t u
--    headOrError u [] = Left $ "Unrecognizeable source URL: " ++ u
--    headOrError _ ((_,f):_) = Right f
--
             
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
