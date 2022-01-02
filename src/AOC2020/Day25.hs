module AOC2020.Day25 where

import Control.Applicative
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

runF =  "data/day25/input.txt"
testF = "data/day25/test.txt"

run = runWith runF
test = runWith testF

load :: FilePath -> IO (Int, Int)
load filePath = do
  [a,b] <- readInputs filePath id
  pure (read a, read b)

runWith :: FilePath -> IO ()
runWith filePath = do
  (ka, kb) <- load filePath
  print (ka, kb)
  let ((loopSize1, value1), (loopSize2, value2)) = findLoopSizes (ka, kb) 7
  print (loopSize1, value1)
  print (loopSize2, value2)
  let encKey1 = opLoop value1 loopSize2
  print encKey1
  let encKey2 = opLoop value2 loopSize1
  print encKey2

findLoopSizes :: (Int, Int) -> Int -> ((Int, Int), (Int, Int))
findLoopSizes (ka, kb) sn =
  let isKey v = v == ka || v == kb
      allLoopSizes = zip [0..] $ iterate' (op sn) 1
      Just (loopSize1, value1) = find (isKey . snd) $ allLoopSizes
      nextLoopSizes = zip [loopSize1..] $ iterate' (op sn) value1
      Just (loopSize2, value2) = find (isKey . snd) $ tail nextLoopSizes
  in  ((loopSize1, value1), (loopSize2, value2))

op :: Int -> Int -> Int
op sn v = (v * sn) `mod` 20201227

opLoop :: Int -> Int -> Int
opLoop sn ls = iterate' (op sn) 1 !! ls
