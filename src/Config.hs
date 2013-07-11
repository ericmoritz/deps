module Config (Config, config, dir, dep_files) where

data Config = Config {
    dir :: String
  , dep_files :: [String]
}

config dir dep_files = Config dir dep_files
