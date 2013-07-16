module Downloaders (DownloadFun) where

import Dep (Dep(Dep))

type DownloadFun = String -> Dep -> IO (Maybe String)
      
