module AOC2020.Day13 where

import qualified Linear as L
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

run = runWith "data/day13/input.txt"
test = runWith "data/day13/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let time = read $ head strs :: Int
      buses = readBuses $ last strs
      buses' = catMaybes buses
      timesUntil = map (\b -> (b, timeUntil time b)) buses'
      (bus, t) = minimumBy (\x y -> compare (snd x) (snd y)) timesUntil
  print (bus * t)

timeUntil :: Int -> Int -> Int
timeUntil time busId = busId - (time `mod` busId)

readBuses :: String -> [Maybe Int]
readBuses = map readBus . splitOn ","

readBus :: String -> Maybe Int
readBus "x" = Nothing
readBus n = Just (read n)
