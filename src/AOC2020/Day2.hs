module AOC2020.Day2 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map

import AOC2020.Common

run = runWith "data/day2/input.txt"
test = runWith "data/day2/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let entries = map readEntry strs
      vc = length . filter id $ map check2 entries
  print vc

type Policy = (Int, Int, Char)
type Entry = (Policy, String)

check :: Entry -> Bool
check ((a, b, c), pw) = a <= cc && cc <= b
  where
    cc = length . filter (== c) $ pw

-- Part 2
check2 :: Entry -> Bool
check2 ((a, b, c), pw) = aa /= bb
  where
    aa = pw !! (a-1) == c
    bb = pw !! (b-1) == c

readEntry :: String -> Entry
readEntry s = (readPolicy policy, password)
  where
    ss = splitOn ": " s
    policy = ss !! 0
    password = ss !! 1

readPolicy :: String -> Policy
readPolicy s = (a, b, c)
  where
    ss = words s
    (a, b) = readRange $ ss !! 0
    [c] = ss !! 1

readRange :: String -> (Int, Int)
readRange s = (a, b)
  where
    ss = splitOn "-" s
    a = read $ ss !! 0
    b = read $ ss !! 1
