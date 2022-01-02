module AOC2020.Day9 where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AOC2020.Common

run = runWith "data/day9/input.txt"
test = runWith "data/day9/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  ints <- readInputs filePath read :: IO [Int]
  let n = 25
      Just i = find (not . valid) $ divvy (n+1) 1 ints
      Just ss = find (not . null) $ map (trySum (last i)) $ tails ints
      s = minimum ss + maximum ss
  -- print (last i) -- part 1
  print s

valid :: [Int] -> Bool
valid xs = l `elem` sums
  where
    (fs, [l]) = splitAt (len - 1) xs
    len = length xs
    sums = catMaybes [ if x == y then Nothing else Just (x + y) | x <- fs, y <- fs ]

trySum :: Int -> [Int] -> [Int]
trySum target xs =
  let i = find ((== target) . snd)
        $ filter ((<= target) . snd)
        $ map (\ys -> (ys, sum ys))
        $ inits xs
  in  case i of
    Nothing -> []
    Just x -> fst x
