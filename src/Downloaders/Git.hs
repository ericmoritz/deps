{-# LANGUAGE FlexibleInstances #-}
module Downloaders.Git (download) where

import Downloaders (DownloadFun)
import Dep (url, name, errorstr)
import Data.List (isPrefixOf, intercalate, stripPrefix)
import Control.Monad (when)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import System.Process (system)
import Debug.Trace (traceShow)

handles :: String -> Bool
handles = ("git+" `isPrefixOf`)

download :: DownloadFun
download dir dep =
  if handles url'
  then do
    exit <- git_clone dep_dir url' .&&. git_checkout dep_dir (ref url')
    deleteIfFailed dep_dir exit 
    return $ Just $ case exit of
      ExitSuccess   -> Right dep_dir
      ExitFailure n -> Left (errorline n)
  else return Nothing
  where
    url' = url dep
    dep_dir = dir ++ (name dep)

    errorline :: Int -> String
    errorline n = errorstr ("git failed with exit code " ++ show n) dep

deleteIfFailed :: FilePath -> ExitCode -> IO ()
deleteIfFailed dir (ExitFailure _) = 
  doesDirectoryExist dir >>= (flip when) (removeDirectoryRecursive dir)
deleteIfFailed _ ExitSuccess = return ()

git_clone :: String -> String -> IO ExitCode
git_clone dir url = do
    run cloneCmd
  where
    cloneCmd = ["git", "clone", git_remote url, dir]

git_checkout :: String -> Maybe String -> IO ExitCode
git_checkout _ Nothing      = return ExitSuccess
git_checkout dir (Just ref) = do
    run checkoutCmd
  where
    checkoutCmd = ["cd", dir, "&&", "git", "checkout", ref]


git_remote :: String -> String
git_remote url = maybe
                 (error "this shouldn't happen...")
                 id
                 (stripPrefix "git+" $ takeWhile (/= '#') url)

ref :: String -> Maybe String
ref url = safeTail $ dropWhile (/= '#') url

run :: [String] -> IO ExitCode
run cmd = do
  system $ intercalate " " cmd
  

safeTail [] = Nothing
safeTail (_:xs) = Just xs

(.&&.) :: IO ExitCode -> IO ExitCode -> IO ExitCode
p1 .&&. p2 = do
  exit1 <- p1
  case exit1 of
    ExitSuccess -> do
      exit2 <- p2
      return exit2
    exit2@(ExitFailure _) ->
      return exit2
      
      
