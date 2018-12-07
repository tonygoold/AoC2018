module Main where

import Data.Char

type Op = Int -> Int
type AccNum = (Int, Int)
type State = ([Op], AccNum)

numAcc :: AccNum -> Int -> AccNum
numAcc (pow, acc) d = (pow * 10, acc + pow * d)

toOps :: Char -> State -> State
toOps c (ops, na@(_, acc))
  | isDigit c = (ops, numAcc na (digitToInt c))
  | c == '+'  = ((\x -> x + acc):ops, (1, 0))
  | c == '-'  = ((\x -> x - acc):ops, (1, 0))
  | otherwise = (ops, na)

findDups :: [Int] -> Int -> [Op] -> Int
findDups ns acc [] = acc
findDups ns acc (op:ops) =
  if acc `elem` ns
    then acc
    else findDups (acc:ns) (op acc) ops

main :: IO ()
main = do
  t <- getContents
  let ops = fst $ foldr toOps ([], (1, 0)) t
  print $ findDups [] 0 (cycle ops)
