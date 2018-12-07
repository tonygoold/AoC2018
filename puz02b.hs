module Main where

import Data.Char

offByOne :: String -> String -> Bool
offByOne (x:xs) (y:ys)
  | x == y    = offByOne xs ys
  | otherwise = xs == ys
offByOne _ _ = False

findMatch :: [String] -> Maybe (String, String)
findMatch (x:y:xs)
  | offByOne x y  = Just (x, y)
  | otherwise     = findMatch (x:xs)
findMatch _ = Nothing

findMatchIter :: [String] -> Maybe (String, String)
findMatchIter (x:xs) = case findMatch (x:xs) of
  Just n -> Just n
  Nothing -> findMatchIter xs
findMatchIter _ = Nothing

main :: IO ()
main = do
  t <- getContents
  print $ findMatchIter $ lines t
