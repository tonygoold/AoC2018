module Main where

import Data.Char
import Data.List
import qualified Data.Map.Lazy as LazyMap

type Checkpair = (Bool, Bool)

toMultisetMap :: Ord a => [a] -> LazyMap.Map a Int
toMultisetMap = foldl incr LazyMap.empty where
  incr m x = LazyMap.insertWith (+) x 1 m

checkpair :: String -> Checkpair
checkpair s = (any (2 ==) vs, any (3 ==) vs) where
  vs = LazyMap.elems $ toMultisetMap s

checksum ps = (length $ findIndices fst ps) * (length $ findIndices snd ps)

main :: IO ()
main = do
  t <- getContents
  let pairs = map checkpair (lines t)
  print $ checksum pairs
