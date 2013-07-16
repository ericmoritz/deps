module Dep (Dep(..)) where

data Dep = Dep {
    name :: String
  , url :: String
  , fileName :: String
  , line :: Int
} deriving (Show)

