module AOC2020.Day6 where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

import AOC2020.Common

run = runWith "data/day6/input.txt"
test = runWith "data/day6/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let groups = readGroups strs
      --groupCounts = map countGroupQs groups
      groupCounts = map count2 groups
      total = sum groupCounts
  print total

countGroupQs :: Group -> Int
countGroupQs = length . nub . concat

count2 :: Group -> Int
count2 = length . foldl1' intersect

type Group = [String]

readGroups :: [String] -> [Group]
readGroups = splitOn [""]
