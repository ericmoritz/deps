{-# LANGUAGE FlexibleInstances #-}
module Downloaders.Git (download) where

import Downloaders (Download)
import Dep (url, name, Dep)
import Data.List (isPrefixOf, intercalate, stripPrefix)
import System.Exit (ExitCode(..))
import System.Process (system)
import Control.Monad
import Control.Monad.Trans

handles :: String -> Bool
handles = ("git+" `isPrefixOf`)

download :: String -> Dep -> Download String
download dir dep = do
  guard (handles url')
  liftIO clone_and_checkout
  return dep_dir
  where
    clone_and_checkout :: IO ExitCode
    clone_and_checkout = git_clone dep_dir url' .&&. git_checkout dep_dir (ref url')
    url' = url dep
    dep_dir = dir ++ (name dep)

git_clone :: String -> String -> IO ExitCode
git_clone dir url = do
    run cloneCmd
    return ExitSuccess
  where
    cloneCmd = ["git", "clone", git_remote url, dir]

git_checkout :: String -> Maybe String -> IO ExitCode
git_checkout _ Nothing      = return ExitSuccess
git_checkout dir (Just ref) = do
    run checkoutCmd
    return ExitSuccess
  where
    checkoutCmd = ["git", "--git-dir=" ++ dir ++ "/.git", "checkout", ref]


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
      
      
