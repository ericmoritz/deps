module Dep (Dep, dep, name, url, DownloadFun) where

type DownloadFun = String -> Dep -> IO String

data Dep = Dep {
    name :: String
  , url :: String
}

dep = Dep