module AOC2020.Day1 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map

import AOC2020.Common

run = runWith "data/day1/input.txt"
test = runWith "data/day1/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let ints = map read strs :: [Int]
      pairs = filter (uncurry (/=)) $ (,) <$> ints <*> ints
      Just (x, y) = find ((== 2020) . uncurry (+)) pairs
  print (x * y)
