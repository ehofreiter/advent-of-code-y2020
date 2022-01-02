module AOC2020.Day12Part2 where

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
      s0 = (L.V2 0 0, L.V2 10 1)
      s1 = foldl' stepState s0 instrs
      (L.V2 x y, d1) = s1
      md = abs x + abs y
  print s1
  print md

stepState :: State -> Instr -> State
stepState (p0, w0) instr = case instr of
  Cardinal (d, n) -> (p0, dirToV2 d L.^* n L.^+^ w0)
  Turn (isLeft, angle) ->
    let quarterTurns = angle `div` 90
        qTurn = if isLeft then leftQTurn else -1 L.*!! leftQTurn
        w1 = foldl' (flip (L.!*)) w0
           $ replicate quarterTurns qTurn
    in  (p0, w1)
  Forward n -> (w0 L.^* n L.^+^ p0, w0)

leftQTurn :: L.M22 Int
leftQTurn = L.V2 (L.V2 0 (-1)) (L.V2 1 0)

dirToV2 :: Dir -> L.V2 Int
dirToV2 E = L.V2 1 0
dirToV2 N = L.V2 0 1
dirToV2 W = L.V2 (-1) 0
dirToV2 S = L.V2 0 (-1)

type State = (L.V2 Int, L.V2 Int) -- ship pos, waypoint pos (relative to ship)

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
