module Downloaders (DownloadFun) where

import Dep (Dep)

type DownloadFun = String -> Dep -> IO (Maybe (Either String String))
      
