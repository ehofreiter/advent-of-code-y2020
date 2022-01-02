module AOC2020.Day3 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map

import AOC2020.Common

run = runWith "data/day3/input.txt"
test = runWith "data/day3/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let rows = map readRow strs
      rl = rowLength (head rows)
      pss = map (paths rl) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
      spss = map (flip spots2 rows) pss
      cs = map (length . filter id) spss
      p = product cs
  print p

-- This one was for Part 1. Assumes that y is incremented by 1.
spots :: [(Int, Int)] -> [Row] -> [Bool]
spots ps rs = zipWith f ps rs
  where
    f (x, y) r = r !! x

-- This was for Part 2, needed to take into account that y can be > 1.
spots2 :: [(Int, Int)] -> [Row] -> [Bool]
spots2 ps rs = map f $ takeWhile ((< l) . snd) ps
  where
    f (x, y) = (rs !! y) !! x
    l = length rs

paths :: Int -> (Int, Int) -> [(Int, Int)]
paths rl (x, y) = iterate' (add rl (x, y)) (0, 0)

add :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
add rl (x1, y1) (x2, y2) = ((x1 + x2) `mod` rl, y1 + y2)

type Row = [Bool]

rowLength :: Row -> Int
rowLength = length

readRow :: String -> Row
readRow = map readSpot

readSpot :: Char -> Bool
readSpot = (== '#')
