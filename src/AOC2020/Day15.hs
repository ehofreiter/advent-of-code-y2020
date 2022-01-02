module AOC2020.Day15 where

import qualified Linear as L
import Control.Applicative
--import Control.Lens
import Data.Bifunctor
import Data.Bits
import Data.Foldable
import Data.Functor.WithIndex
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.IntMap.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AOC2020.Common

run = runWith "data/day15/input.txt"
test = runWith "data/day15/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let ints = map read $ splitOn "," $ head strs :: [Int]
      initState = mkInitState ints
      --x = stepTo 30000000 initState
      --x = step2 initState
      (turn, prev, _) = iterate' step initState !! (30000000 - length ints)
  print (turn, prev)
  --mapM_ print states

type State = (Int, Int, Map.IntMap Int) -- turn, prev, map of numbers to turns spoken

stepTo :: Int -> State -> Int
stepTo n s0 =
  let s1@(turn, prev, _) = step s0
  in  if turn == n + 1
      then prev
      else stepTo n s1

mkInitState :: [Int] -> State
mkInitState ints =
  let numberTurns = Map.fromList $ zip (take (length ints - 1) ints) [1..]
  in  (length ints + 1, last ints, numberTurns)

step :: State -> State
step (turn, prev, numberTurns) = num `seq` (turn+1, num, numberTurns')
  where
    num = case Map.lookup prev numberTurns of
      Nothing -> 0
      Just lastTurn -> turn - 1 - lastTurn
    numberTurns' = Map.insert prev (turn-1) numberTurns

{-
step2 :: State -> Int
step2 (turn, prev, numberTurns) =
  if turn == 30000000
  then num
  else step2 (turn+1, num, numberTurns')
  where
    num = case Map.lookup prev numberTurns of
      Nothing -> 0
      Just lastTurn -> turn - 1 - lastTurn
    numberTurns' = Map.insert prev (turn-1) numberTurns
-}
