module AOC2020.Day24 where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor.WithIndex
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import qualified Text.Parsec as P

import AOC2020.Common

runF =  "data/day24/input.txt"
testF = "data/day24/test.txt"

run = runWith runF
test = runWith testF

load :: FilePath -> IO [Loc]
load filePath = do
  strs <- readInputs filePath id
  pure (map readLoc strs)

runWith :: FilePath -> IO ()
runWith filePath = do
  locs <- load filePath
  print (part1 locs)
  print (part2 locs)

part1 :: [Loc] -> Int
part1 = Map.size . Map.filter (== Black) . flipAll

part2 :: [Loc] -> Int
part2 locs =
  let initialTM = flipAll locs
      finalTM = iterate' conwayFlipAll initialTM !! 100
  in  Map.size $ Map.filter (== Black) finalTM

conwayFlipAll :: TM -> TM
conwayFlipAll tm =
  foldl' (\m c -> Map.insert c (conwayFlip c) m) tm candidates
  where
    touched = Map.keys tm
    candidates = foldl' insertAdj Set.empty touched
    insertAdj s c = foldl' (flip Set.insert) s $ c : adj c
    conwayFlip c =
      let neighbors = map (flip Map.lookup tm) (adj c)
          blackCount = length $ filter (== Just Black) neighbors
      in  case fromMaybe White $ Map.lookup c tm of
        Black | blackCount == 0 || blackCount > 2 -> White
              | otherwise -> Black
        White | blackCount == 2 -> Black
              | otherwise -> White

adj :: (Int, Int) -> [(Int, Int)]
adj c = map (addC c . coordFromDir) [E, W, NE, NW, SE, SW]

flipAll :: [Loc] -> TM
flipAll = foldl' flipAtLoc Map.empty

flipAtLoc :: TM -> Loc -> TM
flipAtLoc tm loc = Map.insertWith (\_ c -> flipC c) (coordFromLoc loc) Black tm

coordFromLoc :: Loc -> (Int, Int)
coordFromLoc = foldl' (\coord dir -> addC coord (coordFromDir dir)) (0, 0)

coordFromDir :: Dir -> (Int, Int)
coordFromDir E  = (2, 0)
coordFromDir W  = (-2, 0)
coordFromDir NE = (1, -1)
coordFromDir NW = (-1, -1)
coordFromDir SE = (1, 1)
coordFromDir SW = (-1, 1)

addC :: (Int, Int) -> (Int, Int) -> (Int, Int)
addC (x0, y0) (x1, y1) = (x0+x1, y0+y1)

data Color = White | Black
  deriving (Eq, Ord, Show)

flipC :: Color -> Color
flipC White = Black
flipC Black = White

type TM = Map.Map (Int, Int) Color

data Dir = E | W | NE | NW | SE | SW
  deriving (Eq, Ord, Show)

type Loc = [Dir]

readLoc :: String -> Loc
readLoc [] = []
readLoc ('e':cs) = E : readLoc cs
readLoc ('w':cs) = W : readLoc cs
readLoc ('n':'e':cs) = NE : readLoc cs
readLoc ('n':'w':cs) = NW : readLoc cs
readLoc ('s':'e':cs) = SE : readLoc cs
readLoc ('s':'w':cs) = SW : readLoc cs
