module Scanner (scan) where

import Control.Monad (liftM)
import Dep (Dep(Dep))

-- Convert a list of dep files into a list of deps
scan :: [FilePath] -> IO [Dep]
scan = liftM concat . mapM mapper
  where mapper fn = liftM (parse_file fn) $ readFile fn

-- ===================================================================
-- Pure Functions
-- ===================================================================
parse_file :: String -> String -> [Dep]
parse_file filename = map line2dep . zip3 (repeat filename) [1..] . lines

line2dep :: (FilePath, Int, String) -> Dep
line2dep (filename, num, line) = do 
    Dep name url filename num
  where
    (name, url) = parse_dep line
             
parse_dep :: String -> (String, String)
parse_dep line = 
  (name, url)
  where
    bits = words line
    name = head bits
    url  = last bits
