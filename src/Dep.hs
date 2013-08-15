module Dep (Dep(..), errorstr) where

data Dep = Dep {
    name :: String
  , url :: String
  , fileName :: String
  , line :: Int
} deriving (Show)

errorstr :: String -> Dep -> String
errorstr msg dep = (fileName dep) ++ ":" ++ show (line dep) ++ ": " ++ msg
