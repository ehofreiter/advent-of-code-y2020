module AOC2020.Day10 where

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

run = runWith "data/day10/input.txt"
test = runWith "data/day10/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  ints <- readInputs filePath read :: IO [Int]
  let diffs = getDiffs (0 : ints ++ [maximum ints + 3])
      ones = length $ filter (== 1) diffs
      threes = length $ filter (== 3) diffs
      m = getSeqsFromChargers ints
  print (ones * threes) -- part 1
  print m -- part 2

getDiffs :: [Int] -> [Int]
getDiffs xs = zipWith (-) (tail sxs) sxs
  where
    sxs = sort xs

-- charger map. maps the charger id (its rating) to the
-- number of ways that it can connect to the outlet.
type CM = Map.Map Int Int

getSeqsFromChargers :: [Int] -> Int
getSeqsFromChargers cs = getSeqs (Map.singleton 0 1) cs'
  where
    cs' = sort cs ++ [maximum cs + 3]
    -- ^ add the outlet (0) and device adapter (max + 3)

getSeqs :: CM -> [Int] -> Int
getSeqs cm [] = cm Map.! maximum (Map.keys cm)
getSeqs cm (c:cs) =
  let compats = Map.filterWithKey f cm
      f ck _ = c - ck <= 3
      cnt = sum compats
      cm' = Map.insert c cnt cm
  in  getSeqs cm' cs
