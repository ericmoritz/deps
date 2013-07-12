module Config (Config, config, dir, enqueue) where
 
import Data.Sequence (Seq, empty, (|>))
import qualified Data.Foldable as F
 
data Config a = Config {
    dir :: String
  , seq :: Seq String
} deriving (Show)
 
config dir = 
  Config dir empty
 
enqueue :: Config -> [String] -> Config
c `enqueue` urls = 
    Config (dir c) q'
  where
    q' = foldl (|>) (Config.seq c) urls
 
-- TODO: make an instance of Foldable?
instance F.Foldable (Config a) where
  foldr f z c =
      Config (dir c) q'
    where
      q' = F.foldr f z (Config.seq c)
 
  foldMap f c = 
      Config (dir c) q'
    where
      q' = F.foldMap f (Config.seq c)