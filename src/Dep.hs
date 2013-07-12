module Dep (Dep(..), DownloadFun, download) where

type DownloadFun = String -> Dep -> IO String

data Dep = Dep {
    name :: String
  , url :: String
  , download_fun :: DownloadFun
}

download dir d = (download_fun d) dir d
