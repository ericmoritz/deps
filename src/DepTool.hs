{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Monad (liftM, filterM, forM_)
import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)
import Downloaders (DownloadFun)
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
  dep_dir <- firstJustM downloads

  return $ case dep_dir of
    Just dep_dir -> Right dep_dir
    Nothing      -> Left $ "Unrecognizeable source URL " ++ fileline ++ url'
  where
    dep_dir   = dir </> (D.name dep)
    downloads = map (\f -> f dir dep) downloaders
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
             
downloaders :: [DownloadFun]
downloaders = [exists, Local.download, Git.download]

-- Dummy downloader for existing directories
exists :: DownloadFun
exists dir dep = do
  putStrLn dep_dir
  exists <- doesDirectoryExist dep_dir
  if exists
    then return $ Just dep_dir
    else return Nothing
  where dep_dir = dir ++ (D.name dep)
        
firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (mx:xs) = mx >>= \x ->
    case x of
      Just _ -> return x
      Nothing -> firstJustM xs
