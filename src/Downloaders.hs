module Downloaders (runDownload, Download) where

import Dep (Dep(Dep))
import Control.Monad.Trans.Maybe

type Download a = MaybeT IO a
      
runDownload :: Download String -> IO (Maybe String)
runDownload = runMaybeT
