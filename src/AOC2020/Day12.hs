module AOC2020.Day12 where

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

run = runWith "data/day12/input.txt"
test = runWith "data/day12/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let instrs = map readInstr strs
      s0 = (L.V2 0 0, E)
      s1 = foldl' stepState s0 instrs
      (L.V2 x y, d1) = s1
      md = abs x + abs y
  print s1
  print md

stepState :: State -> Instr -> State
stepState (p0, d0) instr = case instr of
  Cardinal (d, n) -> (dirToV2 d L.^* n L.^+^ p0, d0)
  Turn (isLeft, angle) ->
    let quarterTurns = if isLeft then angle `div` 90 else - (angle `div` 90)
        d1 = addQuarterTurnsDir d0 quarterTurns
    in  (p0, d1)
  Forward n -> (dirToV2 d0 L.^* n L.^+^ p0, d0)

addQuarterTurnsDir :: Dir -> Int -> Dir
addQuarterTurnsDir d quarterTurns = intToDir $ dirToInt d + quarterTurns

dirToInt :: Dir -> Int
dirToInt E = 0
dirToInt N = 1
dirToInt W = 2
dirToInt S = 3

intToDir :: Int -> Dir
intToDir i = case i `mod` 4 of
  0 -> E
  1 -> N
  2 -> W
  3 -> S

dirToV2 :: Dir -> L.V2 Int
dirToV2 E = L.V2 1 0
dirToV2 N = L.V2 0 1
dirToV2 W = L.V2 (-1) 0
dirToV2 S = L.V2 0 (-1)

type State = (L.V2 Int, Dir) -- position, direction

data Instr
  = Cardinal (Dir, Int)
  | Turn (Bool, Int) -- isLeft, angle
  | Forward Int
  deriving (Eq, Ord, Show)

data Dir = E | N | W | S
  deriving (Eq, Ord, Show)

readInstr :: String -> Instr
readInstr (c:cs) = case c of
  'N' -> Cardinal (N, read cs)
  'E' -> Cardinal (E, read cs)
  'S' -> Cardinal (S, read cs)
  'W' -> Cardinal (W, read cs)
  'L' -> Turn (True, read cs)
  'R' -> Turn (False, read cs)
  'F' -> Forward (read cs)
