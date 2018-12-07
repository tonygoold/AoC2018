module Main where

import Data.Char

main :: IO ()
main = do
  t <- getContents
  print $ fst $ foldr addUp (0, (1, 0)) t where
    addUp c (n, (pow, acc))
      | isDigit c = (n, (pow * 10, acc + pow * digitToInt c))
      | c == '+'  = (n+acc, (1, 0))
      | c == '-'  = (n-acc, (1, 0))
      | otherwise = (n, (pow, acc))
