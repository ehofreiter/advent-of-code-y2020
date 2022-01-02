{-# LANGUAGE BangPatterns #-}
module AOC2020.Day23Part2 where

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
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import qualified Text.Parsec as P

import AOC2020.Common

runF =  "871369452"
testF = "389125467"

run = runWith runF
test = runWith testF

runWith :: String -> IO ()
runWith input = do
  let ints = readInitialCups input
  print ints
  print (part2 ints)

part2 :: [Int] -> Int
part2 ints = part2With (10^7) (initialState (10^6) ints)

part2With :: Int -> State -> Int
part2With r state0 =
  let (_, finalCups) = (!! r) $ iterate' move state0
      a = finalCups V.! 0
      b = finalCups V.! a
  in  (a+1) * (b+1)

type Cups = V.Vector Int
type State = (Int, Cups)

move :: State -> State
move (!current, !cups) =
  let !a = cups V.! current
      b = cups V.! a
      !c = cups V.! b
      !current' = cups V.! c
      !dest = prevLabel current (a,b,c) (V.length cups)
      !dest1 = cups V.! dest
      --cups' = cups V.// [(current, current'), (dest, a), (c, dest1)]
      !cups' = V.modify (\mv -> do
                           MV.write mv current current'
                           MV.write mv dest a
                           MV.write mv c dest1
                       ) cups
  in  (current', cups')

prevLabel :: Int -> (Int, Int, Int) -> Int -> Int
prevLabel n (a, b, c) cupCount =
  head $ drop 1 $ iterate prevInt n \\ [a,b,c]
  where
    prevInt 0 = cupCount - 1
    prevInt n = n - 1

initialState :: Int -> [Int] -> State
initialState n ints = (head ints, mkCups n ints)

readInitialCups :: String -> [Int]
readInitialCups = map (pred . read . (:[]))

mkCups :: Int -> [Int] -> Cups
mkCups n ints = V.generate n f
  where
    start = length ints
    f i | i == n - 1 = head ints
        | i >= start = i + 1
        | otherwise  = case dropWhile (/= i) ints of
            [] -> error "halp"
            [_] -> start
            _:n:_ -> n
