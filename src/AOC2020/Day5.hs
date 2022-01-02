module AOC2020.Day5 where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

import AOC2020.Common

run = runWith "data/day5/input.txt"
test = runWith "data/day5/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let rcs = map readRowCol strs
      rcis = map getRCI rcs
      -- Part 1
      maxID = maximum $ map thd rcis
      -- Part 2
      ids = sort $ map thd rcis
      pairs = zip ids (tail ids)
      nonSeq = filter (\(a, b) -> b - a > 1) pairs
  print nonSeq

thd :: (a, a, a) -> a
thd (_, _, x) = x

readRowCol :: String -> (Row, Col)
readRowCol s = (map fb r, map lr c)
  where
    r = take 7 s
    c = drop 7 s

getRCI :: (Row, Col) -> (Int, Int, Int)
getRCI (row, col) = (r, c, getID r c)
  where
    r = getRow row
    c = getCol col

getRow :: Row -> Int
getRow row = sum $ zipWith f (reverse row) [0..]
  where
    f F i = 0
    f B i = 2^i

getCol :: Col -> Int
getCol row = sum $ zipWith f (reverse row) [0..]
  where
    f L i = 0
    f R i = 2^i

getID :: Int -> Int -> Int
getID r c = r*8 + c

data FB = F | B
  deriving (Eq, Show)
data LR = L | R
  deriving (Eq, Show)

type Row = [FB]
type Col = [LR]

fb :: Char -> FB
fb 'F' = F
fb 'B' = B

lr :: Char -> LR
lr 'L' = L
lr 'R' = R
