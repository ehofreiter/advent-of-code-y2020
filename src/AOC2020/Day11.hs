module AOC2020.Day11 where

import Control.Applicative
--import Control.Lens
import Data.Foldable
import Data.Functor.WithIndex
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AOC2020.Common
import qualified AOC2020.CoordVec as CV

run = runWith "data/day11/input.txt"
test = runWith "data/day11/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let sm = readSM strs
      sm' = stepStable sm
      ss = (map.map) showSpot $ CV.toLists sm'
      c = length . filter (== Full) . CV.toList $ sm'
  mapM_ print ss
  print c

stepStable :: SM -> SM
stepStable sm =
  if sm == sm'
  then sm
  else stepStable sm'
  where
    sm' = stepSM sm

stepN :: Int -> SM -> SM
stepN n | n <= 0 = id
stepN n = stepN (n - 1) . stepSM

stepSM :: SM -> SM
stepSM sm = imap f sm
  where
    f k = stepSpot (CV.adjs8 sm k)

stepSpot :: [Spot] -> Spot -> Spot
stepSpot adj Floor = Floor
stepSpot adj Empty | countFull adj == 0 = Full
stepSpot adj Full  | countFull adj >= 4 = Empty
stepSpot adj s = s

countFull :: [Spot] -> Int
countFull = length . filter (== Full)

data Spot = Floor | Empty | Full
  deriving (Eq, Ord, Show)

type SM = CV.CoordVec Spot

readSM :: [String] -> SM
readSM ss = readSpot <$> CV.fromLists ss

readSpot :: Char -> Spot
readSpot '.' = Floor
readSpot 'L' = Empty
readSpot '#' = Full

showSpot :: Spot -> Char
showSpot Floor = '.'
showSpot Empty = 'L'
showSpot Full = '#'
