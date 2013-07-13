module Scanner (scan) where

import Control.Monad (liftM, mapM)
import Dep (dep, Dep)

-- Convert a list of dep files into a list of deps
scan :: [FilePath] -> IO [Dep]
scan = liftM concat . mapM mapper
  where mapper = liftM parse_file . readFile

-- ===================================================================
-- Pure Functions
-- ===================================================================
parse_file = (lines2deps . lines)

lines2deps :: [String] -> [Dep]
lines2deps = map line2dep
             
line2dep :: String -> Dep
line2dep line = do 
    dep name url
  where
    (name, url) = parse_dep line
             
parse_dep line = 
  (name, url)
  where
    bits = words line
    name = head bits
    url  = last bits
