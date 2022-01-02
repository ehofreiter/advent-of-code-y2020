module AOC2020.Day13Part2 where

import qualified Linear as L
import Control.Applicative
--import Control.Lens
import Data.Bifunctor
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

run = runWith "data/day13/input.txt"
test = runWith "data/day13/test.txt"

-- Tried a naive solution first that essentially brute forced on all multiples
-- of the first bus ID, wasn't efficient enough. Then I spent all day working
-- out a algorithm on paper. Turns out I reinvented the Chinese remainder
-- theorem!

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let buses = readBuses $ last strs
      time = solve buses
      --buses' = iterate solveOne ([], buses)
  print time
  --mapM_ print $ take 10 buses'

solve :: [(Int, Int)] -> Int
solve = loop []
  where
    loop solved buses = case buses of
      []     -> calcTimeFromSolved solved
      (b:bs) -> uncurry loop $ solveOne (solved, buses)

calcTimeFromSolved :: [(Int, Int)] -> Int
calcTimeFromSolved = foldl' f 0
  where
    f time (offset, busId) = offset + busId*time

solveOne :: ([(Int, Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)])
solveOne (solvedBuses, []) = (solvedBuses, [])
solveOne (solvedBuses, (offset, busId):buses) =
  ((offset, busId):solvedBuses, buses')
  where
    buses' = map (findOffset busId . first (subtract offset)) buses

findOffset :: Int -> (Int, Int) -> (Int, Int)
findOffset baseBusId (offset, busId) = (offset', busId)
  where
    offset' = fromJust $ find p [0..busId-1]
    p i = baseBusId * i `mod` busId == offset `mod` busId
    -- ^ may not need to mod offset by busId after first step

readBuses :: String -> [(Int, Int)]
readBuses = mapMaybe f . zip offsets . map readBus . splitOn ","
  where
    offsets = map negate [0..]
    f (i, Nothing) = Nothing
    f (i, Just b) = Just (i, b)

readBus :: String -> Maybe Int
readBus "x" = Nothing
readBus n = Just (read n)
