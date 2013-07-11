module Dep (Dep, new, url, download) where

data Dep = Dep {
  url :: String
  , download_fun :: Dep -> String -> IO String
}

new d f = Dep d f

download d dir =
  (download_fun d) d dir
