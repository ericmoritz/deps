{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Monad (liftM, filterM, forM_)
import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)
import qualified Scanner
import qualified Dep as D
import qualified Downloaders.Local as Local
import qualified Downloaders.Git as Git

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
    download_urls = mapM (download dir)

    dep_files :: [String] -> IO [String]
    dep_files fs    = liftM catMaybes $ mapM dep_file fs


    log_errors :: [Either String (IO String)] -> IO [String]
    log_errors items = do
      forM_ (lefts items) putStrLn
      sequence $ rights items
                           
dep_file :: String -> IO (Maybe String)
dep_file _dep_dir = 
  return Nothing -- TODO: look in dep_dir for a deps.txt

-- ===================================================================
-- Downloaders
-- ===================================================================
-- TODO: move this stuff to Downloaders.Download

-- Downloads the dependancy and returns its directory
download :: String -> D.Dep -> IO (Either String (IO String))
download dir dep =
    downloader url >>= (return . execute)
  where
     url = (D.url dep)
     execute (Left x)  = Left x
     execute (Right f) = Right $ f dir dep
                        

downloader :: String -> IO (Either String D.DownloadFun)
downloader url =
    (return . headOrError url) =<< choose_downloader downloaders url
  where
    choose_downloader ds _ =
      filterM (test url) ds
    test u (t,_) = t u
    headOrError u [] = Left $ "Unrecognizeable source URL: " ++ u
    headOrError _ ((_,f):_) = Right f
             
downloaders :: [((String -> IO Bool), D.DownloadFun)]
downloaders = [
    (Local.is,  Local.download)
  , (Git.is,  Git.download)
  ]

